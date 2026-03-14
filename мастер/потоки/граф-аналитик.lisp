;;; -*- Mode: Lisp; Coding: utf-8 -*-
;;; Имя: поток-граф-аналитик
;;; Описание: принять Mermaid-граф (путь к файлу) → вызвать Python-аналитик → вернуть отчёт

(defpackage :поток-граф-аналитик
  (:use :cl)
  (:export #:выполнить #:выполнить-с-файлом))

(in-package :поток-граф-аналитик)

;; ─── пути ────────────────────────────────────────────────────────────────────

(defparameter *скрипт*
  ;; *корень* определён в мастер/cl/config.lisp → мастер/
  ;; tools/ лежит рядом с мастер/ → поднимаемся на уровень выше
  (let ((мастер (if (find-package :мастер)
                    (symbol-value (intern "*КОРЕНЬ*" :мастер))
                    (truename "."))))
    (merge-pathnames "tools/graph_analyzer/cli.py"
                     (uiop:pathname-parent-directory-pathname мастер))))

(defparameter *конфиг-последнего-запуска*
  "/tmp/граф-аналитик-последний.lisp")

;; ─── состояние ───────────────────────────────────────────────────────────────

(defun сохранить-последний (граф модели)
  (with-open-file (f *конфиг-последнего-запуска*
                     :direction :output :if-exists :supersede
                     :external-format :utf-8)
    (print (list :граф граф :модели модели) f)))

(defun загрузить-последний ()
  (handler-case
      (with-open-file (f *конфиг-последнего-запуска* :external-format :utf-8)
        (let* ((*read-eval* nil)
               (данные (read f nil nil)))
          (when данные
            (values (getf данные :граф)
                    (getf данные :модели)))))
    (error () (values nil nil))))

;; ─── запуск Python ───────────────────────────────────────────────────────────

(defun ключ-апи ()
  (or (sb-ext:posix-getenv "OPENROUTER_API_KEY")
      (error "OPENROUTER_API_KEY не задан")))

(defun запустить-аналитик (граф-путь &optional модели-путь)
  "Запускает python3 graph_analyzer/cli.py и возвращает (values вывод код-возврата)."
  (let* ((args (list "python3" (namestring *скрипт*)
                     граф-путь
                     ;; модели опциональны
                     (or модели-путь "")))
         (args-фильт (remove "" args :test #'string=)))
    (uiop:run-program args-фильт
                      :output :string
                      :error-output :string
                      :ignore-error-status t
                      :environment (list
                                    (format nil "OPENROUTER_API_KEY=~A" (ключ-апи))
                                    (format nil "ANALYZER_MODEL=~A"
                                            (or (sb-ext:posix-getenv "ANALYZER_MODEL")
                                                "anthropic/claude-opus-4-5"))
                                    ;; передаём весь PATH из окружения
                                    (format nil "PATH=~A"
                                            (or (sb-ext:posix-getenv "PATH") "/usr/bin:/usr/local/bin"))))))

;; ─── парсинг аргумента ───────────────────────────────────────────────────────

(defun разобрать-аргумент (задача)
  "Разбирает строку аргументов → (values граф-путь модели-путь-или-nil).
   Форматы:
     /path/to/graph.md
     /path/to/graph.md /path/to/models.py
     повторить  ← повторить последний запуск"
  (let ((части (remove-if (lambda (s) (zerop (length s)))
                           (uiop:split-string (string-trim '(#\Space #\Newline) задача)
                                              :separator '(#\Space)))))
    (cond
      ((null части)
       (values nil nil))
      ((string= (first части) "повторить")
       (загрузить-последний))
      (t
       (values (first части)
               (second части))))))

;; ─── усечение длинного вывода ────────────────────────────────────────────────

(defparameter *макс-символов* 3800
  "Telegram ограничивает сообщение ~4096 символами.")

(defun усечь (текст)
  (if (> (length текст) *макс-символов*)
      (concatenate 'string
                   (subseq текст 0 *макс-символов*)
                   "...\n[отчёт обрезан]")
      текст))

;; ─── точка входа ─────────────────────────────────────────────────────────────

(defun выполнить-с-файлом (граф-путь &optional модели-путь)
  "Основной pipeline: проверка файлов → запуск → ответ."
  (unless (and граф-путь (probe-file граф-путь))
    (return-from выполнить-с-файлом
      (format nil "Файл не найден: ~A" граф-путь)))

  (сохранить-последний граф-путь модели-путь)

  (handler-case
      (multiple-value-bind (вывод ошибки)
          (запустить-аналитик граф-путь модели-путь)
        ;; cli.py пишет всё в stdout включая отчёт
        (let ((текст (string-trim '(#\Space #\Newline)
                                  (if (and вывод (plusp (length вывод)))
                                      вывод
                                      (format nil "Аналитик вернул пустой вывод.~%~A" ошибки)))))
          (усечь текст)))
    (error (e)
      (format nil "Ошибка запуска аналитика: ~A" e))))

(defun выполнить (задача)
  "Точка входа потока. Аргументы: путь к графу [путь к моделям] | 'повторить'."
  (multiple-value-bind (граф модели) (разобрать-аргумент задача)
    (cond
      ((null граф)
       (multiple-value-bind (г м) (загрузить-последний)
         (if г
             (format nil "Использование:~%  /запустить граф-аналитик <граф.md> [модели.py]~%  /запустить граф-аналитик повторить~%~%Последний запуск: ~A~@[ + ~A~]" г м)
             "Использование:~%  /запустить граф-аналитик <путь/к/граф.md> [путь/к/модели.py]")))
      (t
       (выполнить-с-файлом граф модели)))))

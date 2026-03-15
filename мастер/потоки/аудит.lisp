;;; -*- Mode: Lisp; Coding: utf-8 -*-
;;; Имя: аудит
;;; Описание: Поток-верификатор — проверяет репозиторий или diff на нарушения.
;;;           Вход: путь к репо или diff-текст. Выход: t | список нарушений.
;;;           Примеры: credentials в коде, ревью после обновления, gap-анализ.

(defpackage :поток-аудит
  (:use :cl)
  (:export #:выполнить))

(in-package :поток-аудит)

(ql:quickload '("dexador" "cl-json") :silent t)

(defparameter *модель* "openai/gpt-4.1")

(defparameter *промпт-системы*
  "Ты аудитор кода. Проверяй на:
1. Утечки credentials: API-ключи, токены, пароли в коде или конфигах
2. Безопасность: SQL-инъекции, XSS, небезопасные десериализации, eval с пользовательским вводом
3. Архитектурные нарушения: циклические зависимости, нарушение контрактов модулей
4. Стилевые нарушения: глобальные мутабельные состояния, функции длиннее 30 строк

Формат ответа:
Если нарушений нет: ЧИСТО
Если есть нарушения:
НАРУШЕНИЕ: <тип> | <файл:строка> | <описание>
НАРУШЕНИЕ: <тип> | <файл:строка> | <описание>
...
ИТОГО: <число> нарушений

Будь конкретен. Указывай файл и строку. Не выдумывай нарушений.")

(defun ключ-апи ()
  (or (sb-ext:posix-getenv "OPENROUTER_API_KEY") (error "OPENROUTER_API_KEY не задан")))

(defun %запросить (сообщения)
  (handler-case
      (let* ((тело (cl-json:encode-json-to-string
                    `((:model . ,*модель*)
                      (:messages . ,(coerce сообщения 'vector)))))
             (сырой (dexador:post "https://openrouter.ai/api/v1/chat/completions"
                       :content тело
                       :headers `(("Content-Type" . "application/json")
                                  ("Authorization" . ,(format nil "Bearer ~a" (ключ-апи))))
                       :read-timeout 120 :connect-timeout 30))
             (json (cl-json:decode-json-from-string
                    (if (stringp сырой) сырой
                        (sb-ext:octets-to-string сырой :external-format :utf-8)))))
        (or (cdr (assoc :content (cdr (assoc :message (first (cdr (assoc :choices json)))))))
            ""))
    (error (e) (format nil "Ошибка LLM: ~a" e))))

;;; --- сбор контекста ---

(defun %это-репо? (путь)
  "Проверить что путь — директория с .git."
  (and (uiop:directory-exists-p путь)
       (uiop:directory-exists-p (merge-pathnames ".git/" (pathname путь)))))

(defun %git-diff (каталог)
  "git diff HEAD → строка."
  (handler-case
      (uiop:run-program (list "git" "diff" "HEAD")
                        :directory каталог :output :string :error-output nil
                        :ignore-error-status t)
    (error () "")))

(defun %собрать-файлы (каталог расширения макс-файлов)
  "Собрать содержимое файлов с нужными расширениями. Рекурсивно, до макс-файлов."
  (let ((результат '())
        (счетчик 0))
    (labels ((обход (путь)
               (when (< счетчик макс-файлов)
                 (cond
                   ((uiop:directory-exists-p путь)
                    (mapc #'обход (uiop:subdirectories путь))
                    (mapc #'обход (uiop:directory-files путь)))
                   ((and (member (pathname-type путь) расширения :test #'string-equal)
                         (< (or (ignore-errors (with-open-file (f путь) (file-length f))) 0)
                            50000))
                    (handler-case
                        (let ((содержимое (uiop:read-file-string путь)))
                          (push (format nil "=== ~a ===~%~a" (enough-namestring путь каталог) содержимое)
                                результат)
                          (incf счетчик))
                      (error () nil)))))))
      (обход (pathname каталог)))
    (format nil "~{~a~^~%~%~}" (nreverse результат))))

(defun %собрать-контекст (задача)
  "Определить тип входа и собрать контекст для аудита."
  (let ((путь (string-trim '(#\Space #\Newline) задача)))
    (cond
      ;; Путь к репо
      ((%это-репо? путь)
       (let ((diff (%git-diff путь))
             (файлы (%собрать-файлы путь '("lisp" "py" "js" "scm" "sh" "yaml" "json" "toml") 20)))
         (format nil "Репозиторий: ~a~%~%--- GIT DIFF ---~%~a~%~%--- ФАЙЛЫ ---~%~a"
                 путь diff файлы)))
      ;; Путь к файлу
      ((probe-file путь)
       (format nil "Файл: ~a~%~%~a" путь (uiop:read-file-string путь)))
      ;; Текст (diff или код напрямую)
      (t
       (format nil "Код для аудита:~%~a" задача)))))

(defun выполнить (задача)
  "Точка входа. Аудит репо, файла или кода.
   Вход: путь к репо | путь к файлу | текст diff/кода.
   Выход: ЧИСТО | список нарушений."
  (handler-case
      (let* ((контекст (%собрать-контекст задача))
             (сообщения (list `((:role . "system") (:content . ,*промпт-системы*))
                              `((:role . "user") (:content . ,контекст))))
             (ответ (%запросить сообщения)))
        (if (search "ЧИСТО" ответ)
            "✅ Аудит пройден. Нарушений не найдено."
            (format nil "⚠️ Аудит:~%~a" ответ)))
    (error (e) (format nil "Ошибка аудита: ~a" e))))

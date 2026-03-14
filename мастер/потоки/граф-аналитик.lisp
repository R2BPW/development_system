;;; -*- Mode: Lisp; Coding: utf-8 -*-
;;; Имя: поток-граф-аналитик
;;; Описание: Mermaid-граф + Django-модели → LLM → структурированный отчёт
;;; Всё на CL: парсинг, LLM-запросы, форматирование.

(defpackage :поток-граф-аналитик
  (:use :cl)
  (:export #:выполнить #:выполнить-с-файлом))

(in-package :поток-граф-аналитик)

(ql:quickload '("dexador" "cl-json" "cl-ppcre") :silent t)

;; ════════════════════════════════════════════════════════════════
;; Утилиты
;; ════════════════════════════════════════════════════════════════

(defun ключ-апи ()
  (or (sb-ext:posix-getenv "OPENROUTER_API_KEY")
      (error "OPENROUTER_API_KEY не задан")))

(defun модель-апи ()
  (or (sb-ext:posix-getenv "ANALYZER_MODEL")
      "anthropic/claude-opus-4-5"))

(defparameter *конфиг-последнего*
  "/tmp/граф-аналитик-последний.lisp")

(defun сохранить-последний (граф модели)
  (with-open-file (f *конфиг-последнего*
                     :direction :output :if-exists :supersede
                     :external-format :utf-8)
    (print (list :граф граф :модели модели) f)))

(defun загрузить-последний ()
  (handler-case
      (with-open-file (f *конфиг-последнего* :external-format :utf-8)
        (let* ((*read-eval* nil)
               (d (read f nil nil)))
          (values (getf d :граф) (getf d :модели))))
    (error () (values nil nil))))

(defun читать-файл (путь)
  (with-open-file (f путь :external-format :utf-8)
    (let ((buf (make-string (file-length f))))
      (read-sequence buf f)
      buf)))

(defun разбить-строки (текст)
  (uiop:split-string текст :separator '(#\Newline)))

(defun trim (s)
  (string-trim '(#\Space #\Newline #\Return #\Tab) s))

;; ════════════════════════════════════════════════════════════════
;; Парсер Mermaid-графа
;; ════════════════════════════════════════════════════════════════

(defstruct узел
  id метка ошибка? терминал? решение?)

(defstruct ребро
  откуда куда условие)

(defun ошибочный? (id метка)
  (or (cl-ppcre:scan "(?i)ERR" id)
      (cl-ppcre:scan "(?i)(ошибк|компенсац)" метка)))

(defun терминальный? (метка)
  (cl-ppcre:scan "(?i)(финиш|completed|OK\\d|успех|finish)" метка))

(defun разобрать-граф (текст)
  "Mermaid flowchart → (values hash-узлы список-рёбер)."
  (let ((узлы (make-hash-table :test #'equal))
        (рёбра '()))

    ;; Прямоугольные узлы: A0["метка"] или A0[\"метка\"]
    (cl-ppcre:do-register-groups (id метка)
        ("(\\w+)\\[\"([^\"]+)\"\\]" текст)
      (setf (gethash id узлы)
            (make-узел :id id :метка метка
                       :ошибка? (ошибочный? id метка)
                       :терминал? (терминальный? метка))))

    ;; Узлы-решения: A3{"вопрос?"}
    (cl-ppcre:do-register-groups (id метка)
        ("(\\w+)\\{\"([^\"]+)\"\\}" текст)
      (setf (gethash id узлы)
            (make-узел :id id :метка метка :решение? t)))

    ;; Рёбра: A --> B  или  A -->|"условие"| B
    (cl-ppcre:do-register-groups (откуда условие куда)
        ("(\\w+)\\s*-->(?:\\|\"([^\"]*)\"\\|)?\\s*(\\w+)" текст)
      (push (make-ребро :откуда откуда
                        :куда куда
                        :условие (when (and условие (plusp (length условие)))
                                   условие))
            рёбра))

    (values узлы (nreverse рёбра))))

;; ════════════════════════════════════════════════════════════════
;; Парсер Django-моделей
;; ════════════════════════════════════════════════════════════════

(defstruct модель
  имя поля)

(defstruct поле
  имя тип choices? null?)

(defun разобрать-модели (текст)
  "Python-код Django → список структур МОДЕЛЬ."
  (let ((результат '())
        (текущая nil))
    (dolist (строка (разбить-строки текст))
      ;; Новый класс
      (cl-ppcre:register-groups-bind (имя)
          ("^class\\s+(\\w+)\\s*\\(" строка)
        (when текущая
          (push (make-модель :имя (модель-имя текущая)
                             :поля (nreverse (модель-поля текущая)))
                результат))
        (setf текущая (make-модель :имя имя :поля '())))
      ;; Поле модели
      (when текущая
        (cl-ppcre:register-groups-bind (имя тип остаток)
            ("^\\s{4}(\\w+)\\s*=\\s*models\\.(\\w+)\\((.{0,100})" строка)
          (push (make-поле
                 :имя имя
                 :тип тип
                 :choices? (cl-ppcre:scan "choices=" остаток)
                 :null? (cl-ppcre:scan "null=True" остаток))
                (модель-поля текущая)))))
    (when текущая
      (push (make-модель :имя (модель-имя текущая)
                         :поля (nreverse (модель-поля текущая)))
            результат))
    (nreverse результат)))

;; ════════════════════════════════════════════════════════════════
;; Резюме графа и моделей для LLM-промптов
;; ════════════════════════════════════════════════════════════════

(defun узел->строка (у)
  (format nil "~A[~A]~A"
          (узел-id у)
          (subseq (узел-метка у) 0 (min 40 (length (узел-метка у))))
          (cond ((узел-ошибка? у)   " [ERR]")
                ((узел-терминал? у) " [TERM]")
                ((узел-решение? у)  " [?]")
                (t ""))))

(defun резюме-графа (узлы рёбра)
  (let ((обычные '()) (решения '()) (ошибки '()) (терминалы '()))
    (maphash (lambda (k у)
               (declare (ignore k))
               (cond ((узел-ошибка? у)   (push у ошибки))
                     ((узел-терминал? у) (push у терминалы))
                     ((узел-решение? у)  (push у решения))
                     (t                  (push у обычные))))
             узлы)
    (with-output-to-string (s)
      (format s "ГРАФ: ~A узлов, ~A рёбер~%"
              (hash-table-count узлы) (length рёбра))
      (format s "Шаги: ~{~A~^, ~}~%"
              (mapcar #'узел->строка (reverse обычные)))
      (format s "Решения: ~{~A~^, ~}~%"
              (mapcar #'узел->строка (reverse решения)))
      (format s "Ошибки/компенсации: ~{~A~^, ~}~%"
              (mapcar #'узел->строка (reverse ошибки)))
      (format s "Терминалы: ~{~A~^, ~}~%"
              (mapcar #'узел->строка (reverse терминалы)))
      (format s "~%ПЕРЕХОДЫ:~%")
      (dolist (р рёбра)
        (let* ((от (gethash (ребро-откуда р) узлы))
               (к  (gethash (ребро-куда р)  узлы))
               (от-м (if от (узел-метка от) (ребро-откуда р)))
               (к-м  (if к  (узел-метка к)  (ребро-куда р))))
          (format s "  ~A(~A)~A --> ~A(~A)~%"
                  (ребро-откуда р)
                  (subseq от-м 0 (min 25 (length от-м)))
                  (if (ребро-условие р)
                      (format nil " [~A]" (ребро-условие р)) "")
                  (ребро-куда р)
                  (subseq к-м 0 (min 25 (length к-м)))))))))

(defun резюме-моделей (модели)
  (with-output-to-string (s)
    (dolist (м модели)
      (format s "~%class ~A:~%" (модель-имя м))
      (dolist (п (модель-поля м))
        (format s "  ~A: ~A~A~A~%"
                (поле-имя п)
                (поле-тип п)
                (if (поле-choices? п) " [choices]" "")
                (if (поле-null? п) " [nullable]" ""))))))

;; ════════════════════════════════════════════════════════════════
;; Вычисление недостижимых узлов (BFS)
;; ════════════════════════════════════════════════════════════════

(defun недостижимые (узлы рёбра)
  (let ((входящие (make-hash-table :test #'equal))
        (смежность (make-hash-table :test #'equal)))
    (dolist (р рёбра)
      (setf (gethash (ребро-куда р) входящие) t)
      (push (ребро-куда р) (gethash (ребро-откуда р) смежность)))
    (let* ((старты (loop for id being the hash-keys of узлы
                         unless (gethash id входящие) collect id))
           (посещены (make-hash-table :test #'equal))
           (очередь (if старты старты
                        (list (car (loop for id being the hash-keys of узлы
                                         collect id))))))
      (loop while очередь do
        (let ((cur (pop очередь)))
          (unless (gethash cur посещены)
            (setf (gethash cur посещены) t)
            (dolist (next (gethash cur смежность))
              (push next очередь)))))
      (loop for id being the hash-keys of узлы
            unless (gethash id посещены)
            collect (gethash id узлы)))))

;; ════════════════════════════════════════════════════════════════
;; Пути к error-узлам (BFS назад)
;; ════════════════════════════════════════════════════════════════

(defun пути-к-ошибкам (узлы рёбра)
  (let ((обратный (make-hash-table :test #'equal)))
    (dolist (р рёбра)
      (push (ребро-откуда р) (gethash (ребро-куда р) обратный)))
    (with-output-to-string (s)
      (maphash (lambda (k у)
                 (declare (ignore k))
                 (when (узел-ошибка? у)
                   (let ((предшественники '())
                         (посещены (make-hash-table :test #'equal))
                         (очередь (list (узел-id у))))
                     (setf (gethash (узел-id у) посещены) t)
                     (dotimes (_ 4)
                       (let ((следующий '()))
                         (dolist (cur очередь)
                           (dolist (prev (gethash cur обратный))
                             (unless (gethash prev посещены)
                               (setf (gethash prev посещены) t)
                               (push prev предшественники)
                               (push prev следующий))))
                         (setf очередь следующий)))
                   (format s "  ~A[~A] ← предшествующие: ~{~A~^, ~}~%"
                           (узел-id у)
                           (узел-метка у)
                           (reverse предшественники)))))
               узлы))))

;; ════════════════════════════════════════════════════════════════
;; LLM-запросы
;; ════════════════════════════════════════════════════════════════

(defparameter *системный-промпт*
  "Ты — аналитик бизнес-процессов. Тебе дан граф процесса и Django-модели.
Твоя задача — найти конкретные проблемы. Отвечай ТОЛЬКО валидным JSON.
Не выдумывай проблемы которых нет. Каждая находка — реальная, с конкретным узлом графа.
Severity: critical (нужно исправить до релиза), warning (желательно), info (замечание).")

(defparameter *промпты*
  `((:компенсация
     "Граф процесса:
{ГРАФ}

Django-модели:
{МОДЕЛИ}

Пути к error-узлам (предшествующие шаги):
{ОШИБКИ}

Найди пробелы компенсации: для каждого ERR-узла — что уже изменилось в БД
до него и не откатывается. Типично: создали запись → ошибка → запись осталась;
изменили статус → ошибка → статус не восстановлен.

Отвечай JSON:
{\"findings\": [{\"node\": \"ERR1\", \"category\": \"compensation_gap\",
  \"description\": \"...\", \"recommendation\": \"...\", \"severity\": \"critical\"}]}")

    (:крайние-случаи
     "Граф процесса:
{ГРАФ}

Django-модели:
{МОДЕЛИ}

Найди крайние случаи и неожиданные состояния:
1. Граничные входы: пустые списки, null FK, даты в прошлом
2. Комбинации полей модели которые граф не рассматривает
3. Параллельные вызовы одного процесса для одного объекта
4. Partial execution: процесс прервался на середине предыдущего запуска

Отвечай JSON:
{\"findings\": [{\"node\": \"E5\", \"category\": \"edge_case\",
  \"description\": \"...\", \"recommendation\": \"...\", \"severity\": \"warning\"}]}")

    (:достижимость
     "Граф процесса:
{ГРАФ}

Недостижимые узлы (вычислено алгоритмически): {НЕДОСТИЖИМЫЕ}

Найди логические проблемы достижимости:
1. Узлы недостижимые при нормальном потоке (кроме subgraph-группировщиков SG*)
2. Ветки где условие не может быть истинным
3. Циклы которые могут не завершиться

Отвечай JSON:
{\"findings\": [{\"node\": \"F2\", \"category\": \"unreachable_branch\",
  \"description\": \"...\", \"recommendation\": \"...\", \"severity\": \"info\"}]}")))

(defun заменить-плейсхолдеры (шаблон &rest пары)
  "({КЛЮЧ} значение ...) → заменить в шаблоне."
  (let ((результат шаблон))
    (loop for (ключ значение) on пары by #'cddr do
      (setf результат
            (cl-ppcre:regex-replace-all
             (cl-ppcre:quote-meta-chars ключ)
             результат
             (or значение ""))))
    результат))

(defun вызвать-llm (промпт)
  "POST к OpenRouter → строка-ответ."
  (let* ((тело (cl-json:encode-json-to-string
                `((:model . ,(модель-апи))
                  (:messages
                   . (((:role . "system") (:content . ,*системный-промпт*))
                      ((:role . "user")   (:content . ,промпт))))
                  (:max--tokens . 2000))))
         (сырой (dexador:post
                 "https://openrouter.ai/api/v1/chat/completions"
                 :content тело
                 :headers `(("Content-Type"  . "application/json")
                            ("Authorization" . ,(format nil "Bearer ~A" (ключ-апи))))
                 :read-timeout 90 :connect-timeout 30))
         (строка (if (stringp сырой) сырой
                     (sb-ext:octets-to-string сырой :external-format :utf-8))))
    (cdr (assoc :content
                (cdr (assoc :message
                            (car (cdr (assoc :choices
                                            (cl-json:decode-json-from-string строка))))))))))

(defun извлечь-findings (текст)
  "Вытащить findings-список из JSON-ответа LLM."
  (handler-case
      (let* (;; убрать markdown-обёртку
             (чистый (cl-ppcre:regex-replace-all "```(?:json)?\\s*" текст ""))
             (чистый (trim (cl-ppcre:regex-replace-all "```" чистый "")))
             (данные (cl-json:decode-json-from-string чистый)))
        (cdr (assoc :findings данные)))
    (error ()
      ;; попытка найти JSON внутри текста
      (handler-case
          (cl-ppcre:register-groups-bind (json)
              ("(\\{[\\s\\S]*\"findings\"[\\s\\S]*\\})" текст)
            (cdr (assoc :findings (cl-json:decode-json-from-string json))))
        (error () nil)))))

;; ════════════════════════════════════════════════════════════════
;; Форматирование отчёта
;; ════════════════════════════════════════════════════════════════

(defun иконка-серьёзности (severity)
  (cond ((string= severity "critical") "🔴")
        ((string= severity "warning")  "⚠️ ")
        (t                             "ℹ️ ")))

(defun иконка-категории (category)
  (cond ((string= category "compensation_gap")   "[Компенсация]")
        ((string= category "edge_case")           "[Крайний случай]")
        ((string= category "unexpected_state")    "[Состояние]")
        ((string= category "unreachable_branch")  "[Достижимость]")
        (t                                        (format nil "[~A]" category))))

(defun отформатировать-отчёт (findings имя-графа)
  (let ((критичных (count "critical" findings :key (lambda (f) (cdr (assoc :severity f)))
                           :test #'equal))
        (предупреждений (count "warning" findings :key (lambda (f) (cdr (assoc :severity f)))
                               :test #'equal)))
    (with-output-to-string (s)
      (format s "📊 Отчёт: ~A~%" имя-графа)
      (format s "Находок: ~A  (🔴 ~A, ⚠️  ~A, ℹ️  ~A)~%~%"
              (length findings)
              критичных
              предупреждений
              (- (length findings) критичных предупреждений))
      (dolist (f findings)
        (let ((sev  (or (cdr (assoc :severity f)) "info"))
              (cat  (or (cdr (assoc :category f)) "other"))
              (node (or (cdr (assoc :node f)) "?"))
              (desc (or (cdr (assoc :description f)) ""))
              (rec  (cdr (assoc :recommendation f))))
          (format s "~A ~A узел ~A~%   ~A~%~@[   → ~A~%~]~%"
                  (иконка-серьёзности sev)
                  (иконка-категории cat)
                  node
                  desc
                  rec))))))

;; ════════════════════════════════════════════════════════════════
;; Основной pipeline
;; ════════════════════════════════════════════════════════════════

(defparameter *макс-символов* 3800)

(defun усечь (текст)
  (if (> (length текст) *макс-символов*)
      (concatenate 'string (subseq текст 0 *макс-символов*)
                   "...\n[отчёт обрезан]")
      текст))

(defun выполнить-с-файлом (граф-путь &optional модели-путь)
  (unless (probe-file граф-путь)
    (return-from выполнить-с-файлом
      (format nil "Файл не найден: ~A" граф-путь)))

  (сохранить-последний граф-путь модели-путь)

  (handler-case
      (let* ((граф-текст  (читать-файл граф-путь))
             (модели-текст (when (and модели-путь (probe-file модели-путь))
                             (читать-файл модели-путь)))
             (имя (pathname-name (pathname граф-путь))))

        ;; Парсинг
        (multiple-value-bind (узлы рёбра) (разобрать-граф граф-текст)
          (let* ((модели      (if модели-текст (разобрать-модели модели-текст) '()))
                 (г-резюме    (резюме-графа узлы рёбра))
                 (м-резюме    (резюме-моделей модели))
                 (ош-резюме   (пути-к-ошибкам узлы рёбра))
                 (недост      (недостижимые узлы рёбра))
                 (недост-стр  (if недост
                                  (format nil "~{~A~^, ~}"
                                          (mapcar (lambda (у)
                                                    (format nil "~A[~A]"
                                                            (узел-id у)
                                                            (subseq (узел-метка у) 0
                                                                    (min 25 (length (узел-метка у))))))
                                                  недост))
                                  "не обнаружены"))
                 (все-findings '()))

            ;; Три LLM-запроса
            (dolist (шаг *промпты*)
              (let* ((ключ     (first шаг))
                     (шаблон  (second шаг))
                     (промпт  (заменить-плейсхолдеры
                               шаблон
                               "{ГРАФ}"          г-резюме
                               "{МОДЕЛИ}"        м-резюме
                               "{ОШИБКИ}"        ош-резюме
                               "{НЕДОСТИЖИМЫЕ}"  недост-стр)))
                (handler-case
                    (let* ((ответ    (вызвать-llm промпт))
                           (findings (извлечь-findings ответ)))
                      (setf все-findings (append все-findings findings)))
                  (error (e)
                    ;; не падаем — просто пропускаем шаг
                    (format t "; [граф-аналитик] шаг ~A: ~A~%" ключ e)))))

            (усечь (отформатировать-отчёт все-findings имя)))))
    (error (e)
      (format nil "Ошибка анализа: ~A" e))))

;; ════════════════════════════════════════════════════════════════
;; Точка входа потока
;; ════════════════════════════════════════════════════════════════

(defun разобрать-аргумент (задача)
  (let ((части (remove-if (lambda (s) (zerop (length s)))
                           (uiop:split-string (trim задача) :separator '(#\Space)))))
    (cond
      ((null части)           (values nil nil))
      ((string= (first части) "повторить") (загрузить-последний))
      (t (values (first части) (second части))))))

(defun выполнить (задача)
  "Точка входа:
   /запустить граф-аналитик <граф.md> [модели.py]
   /запустить граф-аналитик повторить"
  (multiple-value-bind (граф модели) (разобрать-аргумент задача)
    (cond
      ((null граф)
       (multiple-value-bind (г м) (загрузить-последний)
         (if г
             (format nil "Последний: ~A~@[ + ~A~]~%~%Использование:~%  граф-аналитик <граф.md> [модели.py]~%  граф-аналитик повторить" г м)
             "Использование:~%  граф-аналитик <граф.md> [модели.py]~%  граф-аналитик повторить")))
      (t
       (выполнить-с-файлом граф модели)))))

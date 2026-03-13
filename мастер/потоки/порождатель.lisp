;;;; порождатель.lisp
;;;; Поток, порождающий потоки.
;;;; выполнить: принять описание → сгенерировать .lisp → скомпилировать → вернуть имя.

(defpackage :поток-порождатель
  (:use :cl)
  (:export #:выполнить))

(in-package :поток-порождатель)

(defun ключ-апи ()
  (or (sb-ext:posix-getenv "OPENROUTER_API_KEY") (error "OPENROUTER_API_KEY не задан")))

(defparameter *каталог*
  (or (sb-ext:posix-getenv "STREAMS_DIR")
      (namestring (uiop:pathname-directory-pathname *load-truename*))))

(defparameter *модель* "openai/gpt-4.1")

(defparameter *промпт-системы*
  "Ты генератор Common Lisp потоков. Верни ТОЛЬКО код, без markdown, без ```.
Требования:
- Первая строка: (defpackage :поток-ИМЯ (:use :cl) (:export #:выполнить))
- Вторая строка: (in-package :поток-ИМЯ)
- (defun выполнить (задача) ...) — единственная точка входа, возвращает строку
- HTTP через dexador (уже загружен): (dexador:post url :headers ... :content ...)
  dexador возвращает строку или байты — преобразуй: (if (stringp r) r (sb-ext:octets-to-string r :external-format :utf-8))
- JSON через cl-json (уже загружен): encode-json-to-string, decode-json-from-string
- OPENROUTER_API_KEY через (sb-ext:posix-getenv \"OPENROUTER_API_KEY\")
- OpenRouter URL: https://openrouter.ai/api/v1/chat/completions
- Модель: openai/gpt-4.1
- uiop:run-program для внешних процессов
- handler-case вместо restart-case
- рекурсия вместо loop, функции до 15 строк, без CLOS
- НЕ использовать drakma, flexi-streams, split-sequence — их нет в образе
- НЕ использовать квадратные скобки [] — это не CL синтаксис")

(defun запросить-генерацию (описание)
  (let* ((тело (cl-json:encode-json-to-string
                `((:model . ,*модель*)
                  (:messages . (((:role . "system") (:content . ,*промпт-системы*))
                                ((:role . "user")   (:content . ,(format nil "Создай поток: ~a" описание))))))))
         (raw  (dexador:post "https://openrouter.ai/api/v1/chat/completions"
                             :headers `(("Authorization" . ,(concatenate 'string "Bearer " (ключ-апи)))
                                        ("Content-Type"  . "application/json"))
                             :content тело))
         (str  (if (stringp raw) raw (sb-ext:octets-to-string raw :external-format :utf-8)))
         (resp (cl-json:decode-json-from-string str)))
    (or (cdr (assoc :content (cdr (assoc :message (first (cdr (assoc :choices resp))))))) "")))

(defun извлечь-из-блока (текст)
  "Убирает ```-обёртку если LLM всё-таки добавил."
  (let* ((start (search "```" текст))
         (code-start (when start (position #\Newline текст :start start)))
         (end   (when code-start (search "```" текст :start2 (1+ code-start)))))
    (if (and code-start end)
        (string-trim '(#\Newline #\Space) (subseq текст (1+ code-start) end))
        текст)))

(defun имя-пакета (код)
  (let* ((метка "поток-") (поз (search метка код)))
    (if (null поз) "безымянный"
        (let* ((нач (+ поз (length метка)))
               (кон (or (position-if (lambda (c) (member c '(#\Space #\Newline #\( #\) #\"))) код :start нач)
                        (length код))))
          (subseq код нач кон)))))

(defun записать-поток (имя код)
  (let ((путь (format nil "~a~a.lisp" *каталог* имя)))
    (with-open-file (f путь :direction :output :if-exists :supersede :external-format :utf-8)
      (write-string код f))
    путь))

(defun компилировать (путь)
  (handler-case
      (multiple-value-bind (_ warn fail) (compile-file путь :print nil :verbose nil)
        (declare (ignore _ warn)) (not fail))
    (error () nil)))

(defun загрузить-в-образ (путь)
  (handler-case (progn (load путь :verbose nil :print nil) t) (error () nil)))

(defun выполнить (описание)
  (handler-case
      (let* ((сырой (запросить-генерацию описание))
             (код   (извлечь-из-блока сырой))
             (имя   (имя-пакета код))
             (путь  (записать-поток имя код))
             (ok?   (компилировать путь)))
        (when ok? (загрузить-в-образ путь))
        (format nil "Поток «~a» порождён ~a." имя (if ok? "✅" "⚠️ (compile error)")))
    (error (e) (format nil "Ошибка порождения: ~a" e))))

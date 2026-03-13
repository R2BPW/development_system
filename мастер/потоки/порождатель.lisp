;;;; порождатель.lisp
;;;; Поток, порождающий потоки.
;;;; выполнить: принять описание → сгенерировать .lisp → скомпилировать → вернуть имя.
;;;; Замыкает петлю: генерация живёт в том же слое, что и исполнение.

(defpackage :поток-порождатель
  (:use :cl)
  (:export #:выполнить))

(in-package :поток-порождатель)

;;; --- конфигурация ---

(defun ключ-апи ()
  (or (sb-ext:posix-getenv "OPENROUTER_API_KEY")
      (error "OPENROUTER_API_KEY не задан")))

(defun каталог-потоков ()
  (or (sb-ext:posix-getenv "STREAMS_DIR")
      (namestring
       (merge-pathnames "../мастер/потоки/"
                        (uiop:pathname-directory-pathname *load-truename*)))))

(defparameter *модель* "openai/gpt-4.1")

(defparameter *промпт-системы*
  "Ты генератор вычислительных потоков на Common Lisp.
Верни ТОЛЬКО код Common Lisp без пояснений, без markdown, без ограждений.
Требования к потоку:
- (defpackage :поток-ИМЯ (:use :cl) (:export #:выполнить)) в начале
- (in-package :поток-ИМЯ) после defpackage
- (defun выполнить (задача) ...) — единственная точка входа, возвращает строку
- OPENROUTER_API_KEY через (sb-ext:posix-getenv \"OPENROUTER_API_KEY\")
- HTTP через drakma, JSON через cl-json (уже загружены)
- uiop:run-program для внешних процессов
- handler-case вместо restart-case
- рекурсия вместо loop, функции до 15 строк, без CLOS")

;;; --- HTTP / LLM ---

(defun декодировать (байты)
  (flexi-streams:octets-to-string байты :external-format :utf-8))

(defun запросить-генерацию (описание)
  (let* ((сообщения (list (list (cons "role" "system") (cons "content" *промпт-системы*))
                          (list (cons "role" "user")
                                (cons "content" (format nil "Создай поток: ~a" описание)))))
         (тело      (list (cons "model" *модель*) (cons "messages" сообщения)))
         (ответ     (drakma:http-request "https://openrouter.ai/api/v1/chat/completions"
                      :method :post
                      :content-type "application/json"
                      :additional-headers `(("Authorization" . ,(format nil "Bearer ~a" (ключ-апи))))
                      :content (cl-json:encode-json-alist-to-string тело)
                      :external-format-in  :utf-8
                      :external-format-out :utf-8))
         (данные    (cl-json:decode-json-from-string (декодировать ответ)))
         (choices   (cdr (assoc :choices данные)))
         (сообщение (cdr (assoc :message (car choices)))))
    (or (cdr (assoc :content сообщение)) "")))

;;; --- извлечение кода ---

(defun начало-блока-p (строка)
  (let ((с (string-trim '(#\Space #\Tab) строка)))
    (and (>= (length с) 3) (string= (subseq с 0 3) "```"))))

(defun извлечь-из-блока (строки)
  "Убирает ```-обёртку если есть. Возвращает строку кода."
  (labels ((ищи (ост внутри накоп)
             (cond
               ((null ост)
                (if накоп
                    (format nil "~{~a~%~}" (reverse накоп))
                    nil))
               ((and (not внутри) (начало-блока-p (car ост)))
                (ищи (cdr ост) t накоп))
               ((and внутри (начало-блока-p (car ост)))
                (ищи (cdr ост) nil накоп))
               (внутри
                (ищи (cdr ост) t (cons (car ост) накоп)))
               (t
                (ищи (cdr ост) nil накоп)))))
    (let* ((строки-список (uiop:split-string строки :separator '(#\Newline)))
           (извлечено (ищи строки-список nil nil)))
      (or извлечено строки))))

(defun имя-пакета (код)
  "Извлекает ИМЯ из :поток-ИМЯ через поиск подстроки."
  (let* ((метка  "поток-")
         (поз    (search метка код)))
    (if (null поз)
        "безымянный"
        (let* ((нач (+ поз (length метка)))
               (кон (or (position-if (lambda (c)
                                       (member c '(#\Space #\Newline #\( #\) #\` #\")))
                                     код :start нач)
                        (length код))))
          (subseq код нач кон)))))

;;; --- файловые операции ---

(defun записать-поток (имя код)
  "Пишет код в файл потоков. Возвращает путь."
  (let ((путь (format nil "~a~a.lisp" (каталог-потоков) имя)))
    (with-open-file (f путь :direction :output
                           :if-exists :supersede
                           :external-format :utf-8)
      (write-string код f))
    путь))

(defun записать-мета (имя описание)
  "Пишет .мета файл."
  (let ((путь (format nil "~a~a.мета" (каталог-потоков) имя)))
    (with-open-file (f путь :direction :output
                           :if-exists :supersede
                           :external-format :utf-8)
      (format f "(мета (имя ~s) (описание ~s) (время ~a))"
              имя описание (get-universal-time)))))

(defun компилировать (путь)
  "Компилирует .lisp файл. Возвращает t если успешно."
  (handler-case
      (multiple-value-bind (_ warn fail) (compile-file путь :print nil :verbose nil)
        (declare (ignore _  warn))
        (not fail))
    (error () nil)))

;;; --- загрузить в текущий образ ---

(defun загрузить-в-образ (путь пакет)
  "Загружает поток прямо в текущий образ — без subprocess."
  (handler-case
      (progn
        (load путь :verbose nil :print nil)
        (format t "Загружен в образ: ~a~%" пакет)
        t)
    (error (e)
      (format t "Ошибка загрузки: ~a~%" e)
      nil)))

;;; --- точки входа ---

(defun выполнить (описание)
  "Telegram-точка входа — возвращает строку для пользователя."
  (handler-case
      (let* ((сырой  (запросить-генерацию описание))
             (код    (извлечь-из-блока сырой))
             (имя    (имя-пакета код))
             (путь   (записать-поток имя код))
             (ok?    (компилировать путь))
             (значок (if ok? "✅" "⚠️")))
        (when ok? (загрузить-в-образ путь (format nil "поток-~a" имя)))
        (format nil "Поток «~a» порождён ~a и доступен в образе." имя значок))
    (error (e)
      (format nil "Ошибка порождения: ~a" e))))

(defun запустить-генерацию (описание)
  "CLI-точка входа для Racket-subprocess.
   Выводит в stdout: OK|имя|путь  или  FAIL|сообщение|деталь"
  (handler-case
      (let* ((сырой (запросить-генерацию описание))
             (код   (извлечь-из-блока сырой))
             (имя   (имя-пакета код))
             (путь  (записать-поток имя код))
             (_     (записать-мета имя описание))
             (ok?   (компилировать путь)))
        (when ok? (загрузить-в-образ путь (format nil "поток-~a" имя)))
        (format t "~a|~a|~a" (if ok? "OK" "FAIL") имя путь)
        (finish-output))
    (error (e)
      (format t "FAIL|ошибка|~a" e)
      (finish-output))))

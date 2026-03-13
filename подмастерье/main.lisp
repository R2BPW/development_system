;;;; подмастерье/main.lisp
;;;; Персистентный CL-исполнитель.
;;;; Потоки загружаются в живой образ — не в subprocess.

;;; --- зависимости ---

(let ((ql (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file ql) (load ql :verbose nil :print nil)))

(require :uiop)

(when (find-package :ql)
  (dolist (dep '("drakma" "cl-json" "flexi-streams"))
    (funcall (intern "QUICKLOAD" :ql) dep :silent t)))

(defpackage :подмастерье
  (:use :cl)
  (:export #:запустить))

(in-package :подмастерье)

;;; --- конфигурация ---

(defun ключ-бота ()
  (or (sb-ext:posix-getenv "APPRENTICE_BOT_TOKEN")
      (error "APPRENTICE_BOT_TOKEN не задан")))

(defun ид-админа ()
  (let ((s (sb-ext:posix-getenv "ADMIN_CHAT_ID")))
    (when s (parse-integer s :junk-allowed t))))

(defun админ? (ид)
  (let ((а (ид-админа)))
    (or (null а) (= ид а))))

(defparameter *каталог-потоков*
  (uiop:native-namestring
   (merge-pathnames "../мастер/потоки/"
                    (uiop:pathname-directory-pathname *load-truename*))))

(defparameter *файл-сдвига* "/tmp/apprentice-offset.txt")

;;; --- HTTP / Telegram ---

(defun апи-url (метод)
  (format nil "https://api.telegram.org/bot~a/~a" (ключ-бота) метод))

(defun декодировать (ответ)
  (if (stringp ответ) ответ
      (flexi-streams:octets-to-string ответ :external-format :utf-8)))

(defun послать-сообщение (ид-чата текст)
  (handler-case
      (drakma:http-request (апи-url "sendMessage")
        :method :post
        :content-type "application/json"
        :content (cl-json:encode-json-alist-to-string
                  `(("chat_id" . ,ид-чата) ("text" . ,текст)))
        :external-format-in  :utf-8
        :external-format-out :utf-8)
    (error (e) (format t "Ошибка отправки: ~a~%" e))))

(defun получить-обновления (сдвиг)
  (handler-case
      (декодировать
       (drakma:http-request
         (format nil "~a?timeout=30&offset=~a" (апи-url "getUpdates") сдвиг)
         :external-format-in  :utf-8
         :external-format-out :utf-8))
    (error (e)
      (format t "Ошибка опроса: ~a~%" e)
      "{\"ok\":false,\"result\":[]}")))

;;; --- разбор JSON ---

(defun поле (ключ данные)
  (cdr (assoc ключ данные)))

(defun разобрать-обновления (строка)
  (handler-case
      (let ((д (cl-json:decode-json-from-string строка)))
        (when (поле :ok д) (поле :result д)))
    (error () nil)))

(defun извлечь-сообщения (обновления)
  (remove nil
    (mapcar (lambda (о)
              (let ((с (поле :message о)))
                (when с
                  (list (поле :update-id о)
                        (поле :id (поле :chat с))
                        (or (поле :text с) "")))))
            обновления)))

;;; --- реестр потоков (явное состояние, SICP-style) ---
;;; реестр = alist  (имя-пакета . путь)

(defun прочитать-активный ()
  "Читает активный.scm → (путь пакет) или nil."
  (let ((путь (format nil "~aактивный.scm" *каталог-потоков*)))
    (handler-case
        (with-open-file (f путь)
          (let* ((д     (read f nil nil))
                 (файл  (cadr (assoc 'файл  (cdr д))))
                 (пакет (cadr (assoc 'пакет (cdr д)))))
            (when (and файл пакет) (list файл пакет))))
      (error () nil))))

(defun загрузить-в-образ (путь пакет реестр)
  "Загружает поток в образ если ещё не в реестре. → новый реестр."
  (if (assoc пакет реестр :test #'string=)
      реестр
      (handler-case
          (progn
            (load путь :verbose nil :print nil)
            (format t "Загружен поток: ~a~%" пакет)
            (cons (cons пакет путь) реестр))
        (error (e)
          (format t "Ошибка загрузки ~a: ~a~%" пакет e)
          реестр))))

(defun вызвать-поток (пакет задача)
  "Вызывает ВЫПОЛНИТЬ в загруженном пакете напрямую — без subprocess."
  (handler-case
      (let* ((pkg (find-package (string-upcase пакет)))
             (sym (when pkg (find-symbol "ВЫПОЛНИТЬ" pkg))))
        (if (and sym (fboundp sym))
            (funcall sym задача)
            (format nil "Поток ~a не найден или нет ВЫПОЛНИТЬ." пакет)))
    (error (e) (format nil "Ошибка выполнения: ~a" e))))

;;; --- обработка одного сообщения → (ответ новый-реестр) ---

(defun обработать (текст ид-чата реестр)
  (cond
    ((string= текст "/состояние")
     (let ((актив (прочитать-активный)))
       (values (if актив
                   (format nil "Активный поток: ~a" (second актив))
                   "Поток не загружен.")
               реестр)))
    (t
     (let ((актив (прочитать-активный)))
       (if (null актив)
           (values "Поток не назначен. Используйте Мастера." реестр)
           (let* ((путь       (first актив))
                  (пакет      (second актив))
                  (новый-рег  (загрузить-в-образ путь пакет реестр))
                  (ответ      (вызвать-поток пакет текст)))
             (values ответ новый-рег)))))))

;;; --- цикл (рекурсивный, состояние явное) ---

(defun обр-сообщения (список реестр макс)
  "Рекурсивно обрабатывает список. → (values реестр макс)."
  (if (null список)
      (values реестр макс)
      (let* ((с       (car список))
             (ид-обн  (first с))
             (ид-чата (second с))
             (текст   (third с)))
        (format t "< ~a~%" текст)
        (multiple-value-bind (новый-рег)
            (if (админ? ид-чата)
                (multiple-value-bind (ответ р) (обработать текст ид-чата реестр)
                  (format t "> ~a~%" ответ)
                  (послать-сообщение ид-чата ответ)
                  р)
                (progn (format t "! чужой: ~a~%" ид-чата) реестр))
          (обр-сообщения (cdr список) новый-рег
                          (max макс (1+ ид-обн)))))))

(defun загрузить-сдвиг ()
  (handler-case
      (with-open-file (f *файл-сдвига*)
        (or (parse-integer (string-trim '(#\Space #\Newline) (read-line f nil "0"))
                           :junk-allowed t)
            0))
    (error () 0)))

(defun сохранить-сдвиг (n)
  (with-open-file (f *файл-сдвига* :direction :output :if-exists :supersede)
    (format f "~a" n)))

(defun опрос (сдвиг реестр)
  (handler-case
      (let* ((тело       (получить-обновления сдвиг))
             (обновления (or (разобрать-обновления тело) '()))
             (сообщения  (извлечь-сообщения обновления)))
        (multiple-value-bind (новый-рег новый-сдвиг)
            (обр-сообщения сообщения реестр сдвиг)
          (when (> новый-сдвиг сдвиг) (сохранить-сдвиг новый-сдвиг))
          (опрос новый-сдвиг новый-рег)))
    (error (e)
      (format t "Ошибка цикла: ~a~%" e)
      (sleep 5)
      (опрос сдвиг реестр))))

;;; --- точка входа ---

(defun запустить ()
  (format t "Подмастерье запущен (CL persistent image).~%Потоки живут в образе.~%")
  (опрос (загрузить-сдвиг) '()))

;; Автозапуск если токен задан (иначе файл можно безопасно load-ить из тестов)
(when (sb-ext:posix-getenv "APPRENTICE_BOT_TOKEN")
  (запустить))

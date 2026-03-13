(in-package #:мастер)

;; ─── парсинг ─────────────────────────────────────────────────────────────────

(defun %split-words (str)
  (remove-if #'zerop (uiop:split-string str :separator '(#\Space)) :key #'length))

(defun %cmd-args (text)
  "→ (values команда список-аргументов)"
  (let ((w (%split-words text)))
    (values (if w (string-downcase (first w)) "") (rest w))))

;; ─── обработчики ─────────────────────────────────────────────────────────────

(defparameter *главное-меню*
  (make-reply-keyboard
   '(("Породить поток" "Потоки")
     ("Запустить"      "Состояние")
     ("Диалог"         "Сбросить"))))

(defun %обр-старт (chat-id args)
  (declare (ignore chat-id args))
  (let* ((д     (ignore-errors (читать-душу *путь-души*)))
         (текст (or (and д (%душа-значение д 'указание)) "Мастер готов.")))
    (values текст *главное-меню*)))

(defun %обр-потоки (chat-id args)
  (declare (ignore chat-id args))
  (let ((lst (список-потоков)))
    (if lst (format nil "Потоки: ~{~A~^, ~}" lst) "Нет потоков.")))

(defun %обр-запустить (chat-id args)
  (declare (ignore chat-id))
  (cond
    ((null args)
     (let ((потоки (список-потоков)))
       (if потоки
           (values "Выберите поток:"
                   (make-inline-keyboard
                    (mapcar (lambda (имя)
                              (list (cons имя (format nil "/запустить ~A" имя))))
                            потоки)))
           "Нет доступных потоков.")))
    ((< (length args) 2)
     "Использование: /запустить <поток> <задача>")
    (t
     (or (запустить-поток (first args)
                          (format nil "~{~A~^ ~}" (rest args)))
         (format nil "Ошибка: поток ~A не найден." (first args))))))

(defun %обр-остановить (chat-id args)
  (declare (ignore chat-id args))
  (clrhash *активные-потоки*)
  "Все потоки остановлены.")

(defun %обр-переключить (chat-id args)
  (declare (ignore chat-id))
  (if (null args)
      "Использование: /переключить <поток>"
      (let ((имя (first args)))
        (if (gethash имя *активные-потоки*)
            (progn (remhash имя *активные-потоки*)
                   (format nil "~A остановлен." имя))
            (progn (загрузить-поток
                    (merge-pathnames (format nil "~A.lisp" имя) *каталог-потоков*))
                   (setf (gethash имя *активные-потоки*) t)
                   (format nil "~A активирован." имя))))))

(defun %обр-состояние (chat-id args)
  (declare (ignore chat-id args))
  (let ((lst (активные-потоки)))
    (if lst (format nil "Активные: ~{~A~^, ~}" lst) "Нет активных потоков.")))

(defun %обр-породить (chat-id args)
  (declare (ignore chat-id))
  (if (null args)
      "Использование: /породить <описание>"
      (let ((fn (%найти-выполнить "порождатель")))
        (if fn
            (funcall fn (format nil "~{~A~^ ~}" args))
            "Порождатель не загружен. Сначала: /запустить порождатель ..."))))

(defun %обр-диалог (chat-id args)
  (when (null args) (return-from %обр-диалог "Использование: /диалог <текст>"))
  (let* ((текст    (format nil "~{~A~^ ~}" args))
         (душа     (ignore-errors (читать-душу *путь-души*)))
         (история  (добавить-сообщение chat-id "user" текст))
         (ответ    (llm-complete текст
                                 :system (when душа (душа->системный-промпт душа))
                                 :messages история)))
    (добавить-сообщение chat-id "assistant" ответ)
    ответ))

(defun %обр-сбросить (chat-id args)
  (declare (ignore args))
  (очистить-историю chat-id)
  "Диалог сброшен.")

;; ─── роутинг ─────────────────────────────────────────────────────────────────

(defparameter *команды*
  (list (cons "/старт"       #'%обр-старт)
        (cons "/start"       #'%обр-старт)
        (cons "/потоки"      #'%обр-потоки)
        (cons "/запустить"   #'%обр-запустить)
        (cons "/остановить"  #'%обр-остановить)
        (cons "/переключить" #'%обр-переключить)
        (cons "/состояние"   #'%обр-состояние)
        (cons "/породить"    #'%обр-породить)
        (cons "/диалог"      #'%обр-диалог)
        (cons "/сбросить"    #'%обр-сбросить)))

(defparameter *кнопки*
  '(("Породить поток" . "/породить")
    ("Запустить"      . "/запустить")
    ("Остановить"     . "/остановить")
    ("Потоки"         . "/потоки")
    ("Состояние"      . "/состояние")
    ("Диалог"         . "/диалог")
    ("Сбросить"       . "/сбросить")))

(defun %кнопка->команда (text)
  "Точный матч: кнопка всегда содержит ровно то что в таблице."
  (cdr (assoc text *кнопки* :test #'string=)))

(defun обработать-команду (chat-id text)
  (let* ((t0 (string-trim '(#\Space #\Newline #\Return) text))
         (t1 (or (%кнопка->команда t0) t0)))
    (multiple-value-bind (cmd args) (%cmd-args t1)
      (let ((fn (cdr (assoc cmd *команды* :test #'string=))))
        (if fn
            (handler-case (funcall fn chat-id args)
              (error (e) (format nil "Ошибка: ~A" e)))
            ;; Свободный текст — в диалог
            (%обр-диалог chat-id (list t0)))))))

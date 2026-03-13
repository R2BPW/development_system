(in-package #:мастер)

(defun %split-words (str)
  (let ((result '()) (current (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
    (loop for ch across str do
          (if (char= ch #\Space)
              (when (plusp (length current))
                (push (copy-seq current) result)
                (setf (fill-pointer current) 0))
              (vector-push-extend ch current)))
    (when (plusp (length current)) (push (copy-seq current) result))
    (nreverse result)))

(defun %cmd-args (text)
  "Разбить текст на (cmd . args-list)."
  (let ((words (%split-words text)))
    (values (string-downcase (first words)) (rest words))))

;; --- обработчики ---

(defun %обр-старт (chat-id args)
  (declare (ignore chat-id args))
  (let ((душа (ignore-errors (читать-душу *путь-души*))))
    (format nil "Привет! Я ~A. ~A~%Команды: /потоки /запустить /остановить /состояние /диалог /сбросить"
            (if душа (getf душа :имя) "Мастер")
            (if душа (getf душа :описание) ""))))

(defun %обр-потоки (chat-id args)
  (declare (ignore chat-id args))
  (let ((list (список-потоков)))
    (if list (format nil "Потоки: ~{~A~^, ~}" list) "Нет потоков.")))

(defun %обр-запустить (chat-id args)
  (declare (ignore chat-id))
  (if (< (length args) 2)
      "Использование: /запустить <имя> <задача>"
      (let* ((имя (first args))
             (задача (format nil "~{~A~^ ~}" (rest args)))
             (рез (запустить-поток имя задача)))
        (or рез (format nil "Ошибка запуска ~A" имя)))))

(defun %обр-остановить (chat-id args)
  (declare (ignore chat-id args))
  (clrhash *активные-потоки*)
  "Все потоки остановлены.")

(defun %обр-переключить (chat-id args)
  (declare (ignore chat-id))
  (if (null args)
      "Использование: /переключить <имя>"
      (let ((имя (first args)))
        (if (gethash имя *активные-потоки*)
            (progn (remhash имя *активные-потоки*) (format nil "~A остановлен." имя))
            (progn (загрузить-поток (merge-pathnames (format nil "~A.lisp" имя) *каталог-потоков*))
                   (setf (gethash имя *активные-потоки*) t)
                   (format nil "~A активирован." имя))))))

(defun %обр-состояние (chat-id args)
  (declare (ignore chat-id args))
  (let ((акт (активные-потоки)))
    (if акт (format nil "Активные: ~{~A~^, ~}" акт) "Нет активных потоков.")))

(defun %обр-породить (chat-id args)
  (declare (ignore chat-id))
  (if (null args)
      "Использование: /породить <описание>"
      (let ((описание (format nil "~{~A~^ ~}" args)))
        (handler-case
            (let ((fn (ignore-errors
                        (symbol-function (find-symbol "ВЫПОЛНИТЬ" (find-package "ПОРОЖДАТЕЛЬ"))))))
              (if fn (funcall fn описание) "Порождатель не загружен."))
          (error (e) (format nil "Ошибка: ~A" e))))))

(defun %обр-диалог (chat-id args)
  (let* ((текст (format nil "~{~A~^ ~}" args))
         (душа (ignore-errors (читать-душу *путь-души*)))
         (системный (if душа (душа->системный-промпт душа) nil))
         (история (or (загрузить-историю chat-id) '())))
    (when (plusp (length текст))
      (добавить-сообщение chat-id "user" текст))
    (let* ((resp (llm-complete текст :system системный
                               :messages (загрузить-историю chat-id))))
      (добавить-сообщение chat-id "assistant" resp)
      resp)))

(defun %обр-сбросить (chat-id args)
  (declare (ignore args))
  (очистить-историю chat-id)
  "Диалог сброшен.")

;; --- роутинг ---

(defparameter *команды*
  (list (cons "/старт"      #'%обр-старт)
        (cons "/start"      #'%обр-старт)
        (cons "/потоки"     #'%обр-потоки)
        (cons "/запустить"  #'%обр-запустить)
        (cons "/остановить" #'%обр-остановить)
        (cons "/переключить" #'%обр-переключить)
        (cons "/состояние"  #'%обр-состояние)
        (cons "/породить"   #'%обр-породить)
        (cons "/диалог"     #'%обр-диалог)
        (cons "/сбросить"   #'%обр-сбросить)))

(defun обработать-команду (chat-id text)
  (multiple-value-bind (cmd args) (%cmd-args text)
    (let ((fn (cdr (assoc cmd *команды* :test #'string=))))
      (if fn
          (handler-case (funcall fn chat-id args)
            (error (e) (format nil "Ошибка: ~A" e)))
          (format nil "Неизвестная команда: ~A" cmd)))))

(in-package #:мастер)

(defvar *polling* t)
(defparameter *offset-file* "/tmp/cl-master-offset.txt")

(defun %load-offset ()
  (handler-case
      (with-open-file (f *offset-file*) (read f nil 0))
    (error () 0)))

(defun %save-offset (n)
  (ignore-errors
    (with-open-file (f *offset-file* :direction :output :if-exists :supersede)
      (print n f))))

(defun %drain-offset (saved)
  "Слить всю очередь, начиная с сохранённого offset → следующий offset."
  (labels ((drain (off)
    (let ((upds (ignore-errors (get-updates :timeout 0 :offset off))))
      (if (null upds) off
          (drain (1+ (reduce #'max upds
                             :key (lambda (u) (or (cdr (assoc :update--id u)) 0))
                             :initial-value (1- off))))))))
    (drain saved)))

(defun start ()
  (format t "[мастер] Загружаем потоки...~%")
  (ensure-directories-exist *каталог-истории*)
  (загрузить-все-потоки)
  (let ((offset (%drain-offset (%load-offset))))
    (format t "[мастер] Запущен с offset=~A~%" offset)
    (%save-offset offset)
    (poll-loop offset)))

(defun poll-loop (start-offset)
  (loop with offset = start-offset
        while *polling*
        do (let* ((updates (ignore-errors (get-updates :offset offset :timeout 30)))
                  (max-id  offset))
             (dolist (upd (or updates '()))
               (let ((id (cdr (assoc :update--id upd))))
                 (when (and id (> id max-id)) (setf max-id id)))
               (ignore-errors (обработать-update upd)))
             (when (> max-id offset)
               (setf offset (1+ max-id))
               (%save-offset offset)))))

(defun %chat-id-of (msg)
  (cdr (assoc :id (cdr (assoc :chat msg)))))

(defun %dispatch (chat-id text)
  "Разрешаем доступ только администратору."
  (when (eql chat-id *admin-chat-id*)
    (let ((resp (обработать-команду chat-id text)))
      (when (stringp resp) (send-message chat-id resp)))))

(defun обработать-update (upd)
  (cond
    ((assoc :message upd)
     (let* ((msg  (cdr (assoc :message upd)))
            (text (cdr (assoc :text msg))))
       (when (stringp text)
         (%dispatch (%chat-id-of msg) text))))
    ((assoc :callback--query upd)
     (let* ((cb   (cdr (assoc :callback--query upd)))
            (cid  (%chat-id-of (cdr (assoc :message cb))))
            (data (cdr (assoc :data cb))))
       (answer-callback-query (cdr (assoc :id cb)))
       (%dispatch cid data)))))

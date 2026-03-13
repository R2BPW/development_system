(in-package #:мастер)

(defvar *polling* t)
(defparameter *offset-file* "/tmp/cl-master-offset.txt")

(defun %load-offset ()
  (handler-case
      (with-open-file (f *offset-file*) (read f nil 0))
    (error () 0)))

(defun %save-offset (n)
  (handler-case
      (with-open-file (f *offset-file* :direction :output :if-exists :supersede)
        (print n f))
    (error (e) (log/error "offset" "save: ~A" e))))

(defun %drain-offset (saved)
  "Слить всю очередь, начиная с сохранённого offset → следующий offset."
  (labels ((drain (off)
    (let ((upds (handler-case (get-updates :timeout 0 :offset off)
                  (error (e) (log/error "drain" "~A" e) nil))))
      (if (null upds) off
          (drain (1+ (reduce #'max upds
                             :key (lambda (u) (or (cdr (assoc :update--id u)) 0))
                             :initial-value (1- off))))))))
    (drain saved)))

(defun start ()
  (log/info "мастер" "Загружаем потоки...")
  (ensure-directories-exist *каталог-истории*)
  (загрузить-все-потоки)
  (let ((offset (%drain-offset (%load-offset))))
    (log/info "мастер" "Запущен с offset=~A" offset)
    (%save-offset offset)
    (poll-loop offset)))

(defun poll-loop (start-offset)
  (loop with offset = start-offset
        while *polling*
        do (let ((updates (handler-case (get-updates :offset offset :timeout 30)
                            (error (e) (log/error "poll" "get-updates: ~A" e) nil))))
             (dolist (upd (or updates '()))
               (handler-case (обработать-update upd)
                 (error (e) (log/error "poll" "update: ~A" e)))
               ;; ACK сразу после обработки — offset двигается по каждому апдейту
               (let ((id (cdr (assoc :update--id upd))))
                 (when id
                   (setf offset (1+ id))
                   (%save-offset offset)))))))

(defun %chat-id-of (msg)
  (cdr (assoc :id (cdr (assoc :chat msg)))))

(defun %dispatch (chat-id text)
  "Разрешаем доступ только администратору.
   Обработчики возвращают (values строка &optional reply-markup)."
  (if (or (null *admin-chat-id*) (eql chat-id *admin-chat-id*))
      (multiple-value-bind (текст markup) (обработать-команду chat-id text)
        (when (stringp текст)
          (send-message chat-id текст :reply-markup markup)))
      (send-message chat-id "Доступ запрещён.")))

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

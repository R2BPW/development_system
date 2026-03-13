(in-package #:мастер)

(defvar *polling* t)

(defun start ()
  (format t "[мастер] Загружаем потоки...~%")
  (ensure-directories-exist *каталог-истории*)
  (загрузить-все-потоки)
  (format t "[мастер] Запущен. Ожидаю сообщения...~%")
  (poll-loop 0))

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
               (setf offset (1+ max-id))))))

(defun %chat-id-of (msg)
  (cdr (assoc :id (cdr (assoc :chat msg)))))

(defun обработать-update (upd)
  (cond
    ((assoc :message upd)
     (let* ((msg  (cdr (assoc :message upd)))
            (text (cdr (assoc :text msg))))
       (when (stringp text)
         (send-message (%chat-id-of msg)
                       (обработать-команду (%chat-id-of msg) text)))))
    ((assoc :callback--query upd)
     (let* ((cb   (cdr (assoc :callback--query upd)))
            (cid  (%chat-id-of (cdr (assoc :message cb))))
            (data (cdr (assoc :data cb))))
       (answer-callback-query (cdr (assoc :id cb)))
       (send-message cid (обработать-команду cid data))))))

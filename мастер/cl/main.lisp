(in-package #:мастер)

(defvar *polling* t)

(defun start ()
  (format t "[мастер] Загружаем потоки...~%")
  (ensure-directories-exist *каталог-истории*)
  (загрузить-все-потоки)
  (setf *polling* t)
  (format t "[мастер] Запущен. Ожидаю сообщения...~%")
  (poll-loop 0))

(defun poll-loop (start-offset)
  (loop with offset = start-offset
        while *polling*
        do (let* ((updates (ignore-errors
                             (get-updates :offset offset :timeout 30)))
                  (max-id offset))
             (dolist (upd (or updates '()))
               (let ((id (cdr (assoc :update--id upd))))
                 (when (and id (> id max-id)) (setf max-id id)))
               (ignore-errors (обработать-update upd)))
             (when (> max-id offset)
               (setf offset (1+ max-id))))))

(defun обработать-update (update)
  (cond
    ((assoc :message update)
     (let* ((msg     (cdr (assoc :message update)))
            (chat    (cdr (assoc :chat msg)))
            (chat-id (cdr (assoc :id chat)))
            (text    (cdr (assoc :text msg))))
       (when (stringp text)
         (send-message chat-id (обработать-команду chat-id text)))))
    ((assoc :callback--query update)
     (let* ((cb      (cdr (assoc :callback--query update)))
            (chat-id (cdr (assoc :id (cdr (assoc :chat (cdr (assoc :message cb)))))))
            (data    (cdr (assoc :data cb))))
       (answer-callback-query (cdr (assoc :id cb)))
       (send-message chat-id (обработать-команду chat-id data))))))

(in-package #:мастер)

(defvar *polling* t)

(defun %drain-offset ()
  "Получить текущий максимальный update_id чтобы не переигрывать старые."
  (let ((updates (ignore-errors (get-updates :timeout 0))))
    (if updates
        (1+ (reduce #'max updates
                    :key (lambda (u) (or (cdr (assoc :update--id u)) 0))
                    :initial-value 0))
        0)))

(defun start ()
  (format t "[мастер] Загружаем потоки...~%")
  (ensure-directories-exist *каталог-истории*)
  (загрузить-все-потоки)
  (let ((offset (%drain-offset)))
    (format t "[мастер] Запущен с offset=~A~%" offset)
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
               (setf offset (1+ max-id))))))

(defun %chat-id-of (msg)
  (cdr (assoc :id (cdr (assoc :chat msg)))))

(defun %dispatch (chat-id text)
  (let ((resp (обработать-команду chat-id text)))
    (when (stringp resp) (send-message chat-id resp))))

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

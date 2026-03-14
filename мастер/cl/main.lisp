(in-package #:мастер)

;; Импорт bordeaux-threads с псевдонимом
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :bordeaux-threads))

(defpackage #:bt (:use) (:nicknames :bordeaux-threads))
(import 'bordeaux-threads:make-thread :мастер)

(defvar *polling* t)
(defparameter *offset-file* "/tmp/cl-master-offset.txt")

(defun %load-offset ()
  (handler-case
      (with-open-file (f *offset-file*) (let ((*read-eval* nil)) (read f nil 0)))
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
  ;; Запуск HTTP-сервера в отдельном потоке
  (make-thread #'start-http-server :name "http-server")
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
  (if (or (null *admin-chat-id*) (= chat-id *admin-chat-id*))
      (multiple-value-bind (текст markup) (обработать-команду chat-id text)
        (when (stringp текст)
          (send-message chat-id текст :reply-markup markup)))
      (send-message chat-id "Доступ запрещён.")))

;; ─── обработка входящих документов ──────────────────────────────────────────

(defparameter *входящие-файлы* (make-hash-table :test #'equal)
  "chat-id → путь к последнему скачанному файлу.")

(defun %обр-документ (chat-id msg)
  "Скачать документ, предложить запустить граф-аналитик."
  (let* ((doc      (cdr (assoc :document msg)))
         (file-id  (cdr (assoc :file--id doc)))
         (fname    (or (cdr (assoc :file--name doc)) "файл.md"))
         (local    (format nil "/tmp/мастер-doc-~A-~A"
                           (get-universal-time) fname)))
    (send-message chat-id (format nil "⬇️ Скачиваю ~A..." fname))
    (if (fetch-document file-id local)
        (progn
          (setf (gethash chat-id *входящие-файлы*) local)
          (send-message chat-id
                        (format nil "✅ Сохранён: ~A~%~%Запустить граф-аналитик?" fname)
                        :reply-markup
                        (make-inline-keyboard
                         (list (list
                                (cons "▶️ Анализировать" (format nil "/запустить граф-аналитик ~A" local))
                                (cons "❌ Отмена" "/отмена"))))))
        (send-message chat-id "❌ Не удалось скачать файл."))))

(defun обработать-update (upd)
  (cond
    ((assoc :message upd)
     (let* ((msg  (cdr (assoc :message upd)))
            (text (cdr (assoc :text msg)))
            (cid  (%chat-id-of msg)))
       (cond
         ;; Текстовое сообщение
         ((stringp text)
          (%dispatch cid text))
         ;; Документ (файл)
         ((assoc :document msg)
          (when (or (null *admin-chat-id*) (= cid *admin-chat-id*))
            (%обр-документ cid msg))))))
    ((assoc :callback--query upd)
     (let* ((cb   (cdr (assoc :callback--query upd)))
            (cid  (%chat-id-of (cdr (assoc :message cb))))
            (data (cdr (assoc :data cb))))
       (answer-callback-query (cdr (assoc :id cb)))
       (unless (string= data "/отмена")
         (%dispatch cid data))))))

(in-package #:мастер)

(defparameter *telegram-base* "https://api.telegram.org/bot")

(defun api-url (method)
  (concatenate 'string *telegram-base* *токен* "/" method))

(defun %api-post-real (method payload)
  "POST JSON к Telegram API → alist или nil."
  (handler-case
      (let* ((raw  (dexador:post (api-url method)
                                 :content (cl-json:encode-json-to-string payload)
                                 :headers '(("Content-Type" . "application/json"))))
             ;; dexador может вернуть байты или строку — обрабатываем оба случая
             (str  (if (stringp raw) raw
                       (sb-ext:octets-to-string raw :external-format :utf-8))))
        (cl-json:decode-json-from-string str))
    (error ()
      (log/error "tg" "~A: ошибка API" method)
      nil)))

(defvar *api-post-fn* #'%api-post-real
  "Подменяемая функция для Telegram API POST. Тесты могут подставить заглушку.")

(defun api-post (method payload)
  "Делегирует в *api-post-fn* — позволяет инъекцию для тестов."
  (funcall *api-post-fn* method payload))

(defun get-updates (&key (timeout 30) offset)
  (let ((p (append `((:timeout . ,timeout)) (when offset `((:offset . ,offset))))))
    (cdr (assoc :result (api-post "getUpdates" p)))))

(defun send-message (chat-id text &key reply-markup)
  (when (and text (plusp (length text)))  ; никогда не шлём nil или пустое
    (api-post "sendMessage"
              (append `((:chat--id . ,chat-id) (:text . ,text))
                      (when reply-markup `((:reply--markup . ,reply-markup)))))))

(defun answer-callback-query (id &key text)
  (api-post "answerCallbackQuery"
            (append `((:callback--query--id . ,id))
                    (when text `((:text . ,text))))))

(defun send-document (chat-id path &key caption)
  (handler-case
      (cl-json:decode-json-from-string
       (dexador:post (api-url "sendDocument")
                     :multipart `(("chat_id" . ,(princ-to-string chat-id))
                                  ("document" . ,path)
                                  ,@(when caption `(("caption" . ,caption))))))
    (error () nil)))

;; ─── скачивание файлов ───────────────────────────────────────────────────────

(defun get-file-path (file-id)
  "getFile → file_path (относительный путь на серверах TG)."
  (handler-case
      (let* ((resp (api-post "getFile" `((:file--id . ,file-id))))
             (result (cdr (assoc :result resp))))
        (cdr (assoc :file--path result)))
    (error (e) (log/error "tg" "get-file: ~A" e) nil)))

(defun download-file (file-path local-path)
  "Скачать файл по TG file_path, сохранить в local-path."
  (handler-case
      (let* ((url (format nil "https://api.telegram.org/file/bot~A/~A"
                          *токен* file-path))
             (bytes (dexador:get url :force-binary t)))
        (with-open-file (out local-path
                             :direction :output
                             :element-type '(unsigned-byte 8)
                             :if-exists :supersede)
          (write-sequence bytes out))
        t)
    (error (e) (log/error "tg" "download-file: ~A" e) nil)))

(defun fetch-document (file-id dest-path)
  "Полный цикл: file-id → скачать → dest-path. Возвращает t/nil."
  (let ((fp (get-file-path file-id)))
    (if fp
        (download-file fp dest-path)
        (progn (log/error "tg" "fetch-document: file_path не получен") nil))))

(defun make-reply-keyboard (rows)
  `((:keyboard
     . ,(mapcar (lambda (row)
                  (mapcar (lambda (text) `((:text . ,text))) row))
                rows))
    (:resize--keyboard . t)
    (:persistent . t)))

(defun make-inline-keyboard (rows)
  `((:inline--keyboard
     . ,(mapcar (lambda (row)
                  (mapcar (lambda (b) `((:text . ,(car b)) (:callback--data . ,(cdr b)))) row))
                rows))))

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
      (format *error-output* "[tg] ~A error~%" method)
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

(in-package #:мастер)

(defparameter *telegram-base* "https://api.telegram.org/bot")

(defun api-url (method)
  (concatenate 'string *telegram-base* *токен* "/" method))

(defun api-post (method payload)
  "POST JSON к Telegram API → alist или nil."
  (handler-case
      (let* ((bytes (dexador:post (api-url method)
                                  :content (cl-json:encode-json-to-string payload)
                                  :headers '(("Content-Type" . "application/json"))
                                  :force-binary t))
             (str   (sb-ext:octets-to-string bytes :external-format :utf-8)))
        (cl-json:decode-json-from-string str))
    (error (e)
      (format *error-output* "[tg] ~A: ~A~%" method e)
      nil)))

(defun get-updates (&key (timeout 30) offset)
  (let ((p (append `((:timeout . ,timeout)) (when offset `((:offset . ,offset))))))
    (cdr (assoc :result (api-post "getUpdates" p)))))

(defun send-message (chat-id text &key reply-markup)
  (api-post "sendMessage"
            (append `((:chat--id . ,chat-id) (:text . ,text))
                    (when reply-markup `((:reply--markup . ,reply-markup))))))

(defun answer-callback-query (id &key text)
  (api-post "answerCallbackQuery"
            (append `((:callback--query--id . ,id))
                    (when text `((:text . ,text))))))

(defun send-document (chat-id path &key caption)
  (handler-case
      (cl-json:decode-json-from-string
       (%utf8 (dexador:post (api-url "sendDocument")
                            :multipart `(("chat_id" . ,(princ-to-string chat-id))
                                         ("document" . ,path)
                                         ,@(when caption `(("caption" . ,caption)))))))
    (error (e)
      (format *error-output* "[tg] sendDocument: ~A~%" e)
      nil)))

(defun make-inline-keyboard (rows)
  "rows: ((text . callback-data) ...) список списков."
  `((:inline--keyboard
     . ,(mapcar (lambda (row)
                  (mapcar (lambda (b)
                            `((:text . ,(car b)) (:callback--data . ,(cdr b))))
                          row))
                rows))))

(defun make-reply-keyboard (rows)
  "Persistent reply keyboard. rows: ((text ...) ...) список строк кнопок."
  `((:keyboard
     . ,(mapcar (lambda (row)
                  (mapcar (lambda (text) `((:text . ,text))) row))
                rows))
    (:resize--keyboard . t)
    (:persistent . t)))

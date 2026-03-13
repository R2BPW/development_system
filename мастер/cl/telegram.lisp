(in-package #:мастер)

(defparameter *telegram-api-base* "https://api.telegram.org/bot")

(defun api-url (method)
  (concatenate 'string *telegram-api-base* *токен* "/" method))

(defun api-post (method payload)
  "POST JSON payload к Telegram API. Возвращает распарсенный alist или nil."
  (handler-case
      (let* ((body (cl-json:encode-json-to-string payload))
             (resp (dexador:post (api-url method)
                                 :content body
                                 :headers '(("Content-Type" . "application/json")))))
        (cl-json:decode-json-from-string resp))
    (error (e)
      (format *error-output* "[telegram] ошибка ~A: ~A~%" method e)
      nil)))

(defun get-updates (&key (timeout 30) offset)
  "Long-poll Telegram. Возвращает список update-alist или nil."
  (let* ((params (append `((:timeout . ,timeout))
                         (when offset `((:offset . ,offset)))))
         (resp (api-post "getUpdates" params)))
    (when resp (cdr (assoc :result resp)))))

(defun send-message (chat-id text &key reply-markup (retries 2))
  (let ((payload (append `((:chat--id . ,chat-id) (:text . ,text))
                         (when reply-markup `((:reply--markup . ,reply-markup))))))
    (handler-case
        (api-post "sendMessage" payload)
      (error (e)
        (when (plusp retries)
          (sleep 5)
          (send-message chat-id text
                        :reply-markup reply-markup
                        :retries (1- retries)))))))

(defun send-document (chat-id filepath &key caption)
  "Отправить файл через multipart (упрощённо через content-file)."
  (handler-case
      (let ((resp (dexador:post (api-url "sendDocument")
                                :multipart `(("chat_id" . ,(princ-to-string chat-id))
                                             ,@(when caption `(("caption" . ,caption)))
                                             ("document" . ,filepath)))))
        (cl-json:decode-json-from-string resp))
    (error (e)
      (format *error-output* "[telegram] sendDocument ошибка: ~A~%" e)
      nil)))

(defun answer-callback-query (callback-id &key text)
  (let ((payload (append `((:callback--query--id . ,callback-id))
                         (when text `((:text . ,text))))))
    (api-post "answerCallbackQuery" payload)))

(defun make-inline-keyboard (rows)
  "rows: список строк, каждая строка — список (text . callback-data)."
  (let ((kb (mapcar (lambda (row)
                      (mapcar (lambda (btn)
                                `((:text . ,(car btn)) (:callback--data . ,(cdr btn))))
                              row))
                    rows)))
    `((:inline--keyboard . ,kb))))

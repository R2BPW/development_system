(in-package #:мастер)

(defparameter *telegram-api-base* "https://api.telegram.org/bot")

(defun api-url (&rest parts)
  (apply #'concatenate 'string *telegram-api-base* *токен* "/" (mapcar #'identity parts)))

(defun api-call (method &key (params nil) (files nil))
  (let ((url (api-url method)))
    (handler-case
        (if files
            (dexador:post url :parameters params :files files)
            (dexador:post url :parameters params))
      (error (e) (format nil "API error: ~A" e)))))

(defun get-updates (&key (timeout 30) (offset nil))
  (let ((params (remove-if #'null `((timeout . ,timeout)
                                    (offset . ,offset)))))
    (let ((resp (api-call "getUpdates" :params params)))
      (when (and resp (stringp resp))
        (cdr (assoc :result (cl-json:decode-json-from-string resp)))))))

(defun send-message (chat-id text &key reply-markup)
  (let ((params `((chat_id . ,chat-id)
                  (text . ,text)
                  ,@(when reply-markup `((reply_markup . ,reply-markup))))))
    (api-call "sendMessage" :params params)))

(defun send-document (chat-id filepath &key (caption nil))
  (let ((params `((chat_id . ,chat-id)
                  ,@(when caption `((caption . ,caption)))))
        (files `(("document" . ,filepath))))
    (api-call "sendDocument" :params params :files files)))

(defun answer-callback-query (callback-query-id &key (text nil))
  (let ((params `((callback_query_id . ,callback-query-id)
                  ,@(when text `((text . ,text))))))
    (api-call "answerCallbackQuery" :params params)))

(defun make-inline-keyboard (buttons)
  ;; buttons: list of lists [[({:text :callback_data} ...)] ...]
  (cl-json:encode-json-to-string
    `((inline_keyboard . ,buttons))))

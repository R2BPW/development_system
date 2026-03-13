(in-package #:мастер)

(defparameter *llm-url*   "https://openrouter.ai/api/v1/chat/completions")
(defparameter *llm-model* "openai/gpt-4.1")
(defparameter *llm-key*   (uiop:getenv "OPENROUTER_API_KEY"))

(defun %auth-headers ()
  `(("Authorization" . ,(format nil "Bearer ~a" *llm-key*))
    ("Content-Type"  . "application/json")))

(defun %build-messages (prompt &key system messages)
  (append (when system   `((("role" . "system")    ("content" . ,system))))
          (or messages `((("role" . "user")      ("content" . ,prompt))))))

(defun llm-complete (prompt &key (model *llm-model*) system messages)
  "Запрос к LLM → строка-ответ. Никогда не бросает."
  (handler-case
      (let* ((body  (cl-json:encode-json-to-string
                     `(("model" . ,model)
                       ("messages" . ,(%build-messages prompt :system system :messages messages)))))
             (resp  (dexador:post *llm-url* :content body :headers (%auth-headers)))
             (json  (cl-json:decode-json-from-string resp))
             (msg   (cdr (assoc :message (first (cdr (assoc :choices json)))))))
        (or (cdr (assoc :content msg)) "[нет ответа]"))
    (error (e)
      (format *error-output* "[llm] ~A~%" e)
      "[ошибка LLM]")))

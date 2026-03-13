(in-package #:мастер)

(defparameter *llm-url*     "https://openrouter.ai/api/v1/chat/completions")
(defparameter *llm-model*   "openai/gpt-4.1")
(defparameter *llm-timeout* 60)
(defvar       *llm-key*     nil)

(defun %llm-key ()
  "Ленивое чтение ключа — работает если ENV задан после загрузки."
  (or *llm-key* (setf *llm-key* (uiop:getenv "OPENROUTER_API_KEY"))))

(defun %auth-headers ()
  `(("Authorization" . ,(format nil "Bearer ~a" (%llm-key)))
    ("Content-Type"  . "application/json")))

(defun %build-messages (prompt &key system messages)
  (append (when system   `((("role" . "system")    ("content" . ,system))))
          (or messages `((("role" . "user")      ("content" . ,prompt))))))

(defun %llm-complete-real (prompt &key (model *llm-model*) system messages)
  "Реальный запрос к LLM → строка-ответ. Никогда не бросает."
  (handler-case
      (let* ((body  (cl-json:encode-json-to-string
                     `(("model" . ,model)
                       ("messages" . ,(%build-messages prompt :system system :messages messages)))))
             (resp  (dexador:post *llm-url*
                                  :content body
                                  :headers (%auth-headers)
                                  :read-timeout *llm-timeout*
                                  :connect-timeout *llm-timeout*))
             (json  (cl-json:decode-json-from-string resp))
             (msg   (cdr (assoc :message (first (cdr (assoc :choices json)))))))
        (or (cdr (assoc :content msg)) "[нет ответа]"))
    (error (e)
      (format *error-output* "[llm] ~A~%" e)
      "[ошибка LLM]")))

(defvar *llm-fn* #'%llm-complete-real
  "Подменяемая функция для LLM. Тесты могут подставить заглушку.")

(defun llm-complete (prompt &key (model *llm-model*) system messages)
  "Делегирует в *llm-fn* — позволяет инъекцию для тестов."
  (funcall *llm-fn* prompt :model model :system system :messages messages))

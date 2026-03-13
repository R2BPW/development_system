;;; llm.lisp — OpenRouter API interface for LLM completion
(in-package #:мастер)

(defparameter *openrouter-api-url* "https://openrouter.ai/api/v1/chat/completions")
(defparameter *openrouter-model* "openai/gpt-4.1")
(defparameter *openrouter-key* (uiop:getenv "OPENROUTER_API_KEY"))

(defun llm-complete (prompt &key (model *openrouter-model*) system messages)
  "Send prompt to OpenRouter LLM, return a string completion. system/messages are optional."
  (let* ((body-json (cl-json:encode-json-to-string
                     `(("model" . ,model)
                       ("messages" . ,(append
                          (when system
                            (list (list :role "system" :content system)))
                          (if messages
                              messages
                              (list (list :role "user" :content prompt))))))))
         (headers `(("Authorization" . ,(format nil "Bearer ~a" *openrouter-key*))
                    ("Content-Type" . "application/json")))
         (response (dexador:http-post *openrouter-api-url*
                                      :content body-json
                                      :headers headers)))
    ;; Response is a string. Parse JSON.
    (let* ((json (ignore-errors (cl-json:decode-json-from-string response)))
           (choices (cdr (assoc :choices json))))
      (if (and choices (consp choices))
          (let ((msg (cdr (assoc :message (first choices)))))
            (or (cdr (assoc :content msg))
                "[LLM: нет ответа]"))
          "[LLM: нет ответа]"))))

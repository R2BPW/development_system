(defpackage :поток-выполни-код
  (:use :cl)
  (:export #:выполнить))
(in-package :поток-выполни-код)

(defun %ключ () (sb-ext:posix-getenv "OPENROUTER_API_KEY"))

(defun %запрос-llm (задача)
  (let* ((payload (cl-json:encode-json-to-string
                   `((:model . "openai/gpt-4.1")
                     (:messages . (((:role . "user")
                                    (:content . ,(format nil "Напиши Python-код который: ~A~%Верни ТОЛЬКО код в ```python блоке." задача))))))))
         (raw (dexador:post "https://openrouter.ai/api/v1/chat/completions"
                            :headers `(("Authorization" . ,(concatenate 'string "Bearer " (%ключ)))
                                       ("Content-Type" . "application/json"))
                            :content payload))
         (str (if (stringp raw) raw (sb-ext:octets-to-string raw :external-format :utf-8)))
         (resp (cl-json:decode-json-from-string str))
         (content (cdr (assoc :content
                              (cdr (assoc :message
                                         (first (cdr (assoc :choices resp)))))))))
    content))

(defun %извлечь-python (текст)
  (let* ((start (search "```python" текст))
         (begin (when start (+ start 9)))
         (end   (when begin (search "```" текст :start2 begin))))
    (if (and begin end) (string-trim '(#\Newline #\Space) (subseq текст begin end)) текст)))

(defun %запустить-python (код)
  (let ((файл "/tmp/выполни_код.py"))
    (with-open-file (f файл :direction :output :if-exists :supersede)
      (write-string код f))
    (handler-case
        (nth-value 0 (uiop:run-program (list "python3" файл)
                                       :output :string :ignore-error-status t))
      (error (e) (format nil "Ошибка запуска: ~a" e)))))

(defun выполнить (задача)
  (handler-case
      (let* ((ответ   (%запрос-llm задача))
             (код     (%извлечь-python ответ))
             (вывод   (%запустить-python код)))
        (if (plusp (length вывод)) вывод "Код не вывел результат"))
    (error (e) (format nil "Ошибка: ~a" e))))

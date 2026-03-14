(defpackage :поток-для-выполнения-кода (:use :cl) (:export #:выполнить))
(in-package :поток-для-выполнения-кода)

(defun сформировать-запрос (задача)
  (encode-json-to-string
   `(("model" . "openai/gpt-4.1")
     ("messages" . ,(list (list "role" "system" "content" "На входе задача, ответь ТОЛЬКО корректным кодом без пояснений, строго на Python.") 
                          (list "role" "user" "content" задача)))
     ("max_tokens" . 1024))))

(defun отправить-запрос (json)
  (let* ((url "https://openrouter.ai/api/v1/chat/completions")
         (headers `(("Authorization" . ,(concatenate 'string "Bearer " (sb-ext:posix-getenv "OPENROUTER_API_KEY")))
                    ("Content-Type" . "application/json")))
         (r (dexador:post url :headers headers :content json)))
    (if (stringp r) r (sb-ext:octets-to-string r :external-format :utf-8))))

(defun извлечь-код (ответ)
  (let* ((json (decode-json-from-string ответ))
         (choices (cdr (assoc "choices" json)))
         (content (cdr (assoc "content" (cdr (assoc "message" (car choices)))))))
    (преобразовать-код content)))

(defun преобразовать-код (стр)
  (labels ((рек (lst acc начало конец)
             (cond
               ((null lst) (if acc (string-trim '(#\Newline #\Space #\Return) (coerce (nreverse acc) 'string)) nil))
               ((and (char= (car lst) #\`)
                     (>= (length lst) 3)
                     (char= (nth 1 lst) #\`)
                     (char= (nth 2 lst) #\`))
                (if начало
                    (рек (cdddr lst) acc nil t)
                    (рек (cdddr lst) acc t конец)))
               (начало (рек (cdr lst) (cons (car lst) acc) начало конец))
               (t (рек (cdr lst) acc начало конец)))))
    (let ((рез (рек (coerce стр 'list) nil nil nil)))
      (if рез рез стр))))

(defun выполнить-python-код (code)
  (handler-case
      (let* ((res (uiop:run-program '("python3" "-c" code)
                                    :output :string :error-output :string :ignore-error-status t)))
        (if (> (length (uiop:process-info-error-output res)) 0)
            (format nil "Ошибка выполнения Python:~%~A" (uiop:process-info-error-output res))
            (uiop:process-info-output res)))
    (error (e) (format nil "Ошибка исполнения внешнего процесса: ~A" e))))

(defun выполнить (задача)
  (handler-case
      (let* ((json-запрос (сформировать-запрос задача))
             (ответ (отправить-запрос json-запрос))
             (код (извлечь-код ответ)))
        (если-пустой-код-то-ошибка код (выполнить-python-код код)))
    (error (e) (format nil "Ошибка: ~A" e))))

(defun если-пустой-код-то-ошибка (код результат)
  (if (or (null код) (eql (length (string-trim '(#\Space #\Newline #\Return) код)) 0))
      "Языковая модель не вернула код."
      результат))
(defpackage :поток-выполнить-питон (:use :cl) (:export #:выполнить))
(in-package :поток-выполнить-питон)

(defun http-post-json (url data api-key)
  (let ((headers `(("Authorization" . ,(concatenate 'string "Bearer " api-key))
                   ("Content-Type" . "application/json"))))
    (drakma:http-request url
                        :method :post
                        :content (cl-json:encode-json-to-string data)
                        :additional-headers headers)))

(defun получить-код-питон (задача)
  (let* ((api "https://openrouter.ai/api/v1/chat/completions")
         (key (sb-ext:posix-getenv "OPENROUTER_API_KEY"))
         (запрос `(("model" . "openai/gpt-3.5-turbo")
                   ("messages" . ((("role" . "system")
                                   ("content" . "Тебе дадут задание на естественном языке. Ответь только минимальным рабочим Python-кодом для решения задачи, без пояснений."))
                                  (("role" . "user") ("content" . ,задача)))))))
    (handler-case
        (let ((resp (http-post-json api запрос key)))
          (let* ((стр-json (babel:octets-to-string resp :encoding :utf-8))
                 (json (cl-json:decode-json-from-string стр-json)))
            (cdr (assoc 'content (aref (cdr (assoc 'choices json)) 0)))))
      (error (e) (format nil "Ошибка получения кода: ~A" e)))))

(defun записать-в-файл-рекурс (строка поток i)
  (if (>= i (length строка))
      nil
      (progn
        (write-char (char строка i) поток)
        (записать-в-файл-рекурс строка поток (1+ i)))))

(defun записать-в-файл (код путь)
  (with-open-file (ф путь :direction :output :if-exists :supersede)
    (записать-в-файл-рекурс код ф 0)))

(defun выполнить-питон (код)
  (let ((путь "/tmp/openrouter_py_code.py"))
    (handler-case
        (progn
          (записать-в-файл код путь)
          (multiple-value-bind (вывод код-статус)
              (uiop:run-program (list "python3" путь) :output :string :ignore-error-status t)
            (if (= код-статус 0)
                вывод
                (format nil "Ошибка исполнения Python: ~A" вывод))))
      (error (e) (format nil "Ошибка записи/исполнения: ~A" e)))))

(defun почистить-код (код i лимит)
  (cond ((or (>= i (length код)) (>= i лимит)) "")
        ((and (char= (char код i) #'\`))
         (почистить-код код (1+ i) лимит))
        (t (concatenate 'string (string (char код i))
                        (почистить-код код (1+ i) лимит)))))

(defun обработать-ответ-кода (ответ)
  (let ((найден (search "```python" ответ)))
    (if найден
        (let* ((начало (+ найден 9))
               (конец (search "```" ответ :start2 начало)))
          (subseq ответ начало (or конец (length ответ))))
        (почистить-код ответ 0 3000))))

(defun выполнить (задача)
  (handler-case
      (let* ((ответ (получить-код-питон задача))
             (код (обработать-ответ-кода ответ)))
        (выполнить-питон код))
    (error (e) (format nil "Ошибка потока: ~A" e))))
;;; -*- Mode: Lisp -*-
;;; Имя: поток-питон
;;; Описание: принять задачу, спросить модель о Python-коде, выполнить, вернуть вывод

(defpackage :поток-питон
  (:use :cl)
  (:export #:выполнить))

(in-package :поток-питон)

(ql:quickload '("drakma" "cl-json" "flexi-streams") :silent t)

(defun ключ-апи ()
  (or (sb-ext:posix-getenv "OPENROUTER_API_KEY")
      (error "OPENROUTER_API_KEY не задан")))

(defun собрать-сообщения (задача)
  (list `((:role . "system")
          (:content . "Ты генератор Python-кода. Верни ТОЛЬКО код без пояснений, без markdown. Код выводит результат через print."))
        `((:role . "user")
          (:content . ,(format nil "Напиши Python-код для задачи: ~A" задача)))))

(defun тело-запроса (задача)
  (cl-json:encode-json-to-string
   `((:model . "openai/gpt-4.1")
     (:messages . ,(собрать-сообщения задача)))))

(defun запросить-модель (задача)
  (let ((ответ (drakma:http-request
                "https://openrouter.ai/api/v1/chat/completions"
                :method :post
                :content-type "application/json"
                :additional-headers
                `(("Authorization" . ,(format nil "Bearer ~A" (ключ-апи))))
                :content (тело-запроса задача)
                :want-stream nil)))
    (if (typep ответ '(vector (unsigned-byte 8)))
        (flexi-streams:octets-to-string ответ :external-format :utf-8)
        ответ)))

(defun извлечь-код (ответ-json)
  (let* ((разбор (cl-json:decode-json-from-string ответ-json))
         (выборы (cdr (assoc :choices разбор)))
         (сообщение (cdr (assoc :message (car выборы))))
         (содержимое (cdr (assoc :content сообщение))))
    (очистить-код содержимое)))

(defun очистить-код (текст)
  (if (null текст) ""
      (let ((ч (string-trim '(#\Space #\Newline #\Return) текст)))
        (cond ((и-префикс-p "```python" ч) (убрать-обёртку ч))
              ((и-префикс-p "```" ч) (убрать-обёртку ч))
              (t ч)))))

(defun и-префикс-p (префикс строка)
  (and (>= (length строка) (length префикс))
       (string= префикс (subseq строка 0 (length префикс)))))

(defun убрать-обёртку (текст)
  (let* ((поз (position #\Newline текст))
         (без-начала (if поз (subseq текст (1+ поз)) текст))
         (конец (search "```" без-начала :from-end t)))
    (string-trim '(#\Space #\Newline #\Return)
                 (if конец (subseq без-начала 0 конец) без-начала))))

(defun выполнить-питон (код)
  (let ((путь (format nil "/tmp/поток-питон-~A.py" (get-universal-time))))
    (with-open-file (п путь :direction :output
                            :if-exists :supersede
                            :external-format :utf-8)
      (write-string код п))
    (handler-case
        (multiple-value-bind (вывод ошибки код-возврата)
            (uiop:run-program (list "python3" путь)
                              :output :string
                              :error-output :string
                              :ignore-error-status t)
          (if (zerop код-возврата)
              (string-trim '(#\Space #\Newline #\Return) вывод)
              (format nil "Ошибка (код ~A): ~A" код-возврата ошибки)))
      (error (е) (format nil "Ошибка запуска: ~A" е)))))

(defun выполнить (задача)
  (when (find-package :трас)
    (funcall (intern "НАЧАТЬ-СЛЕД" :трас) "поток-питон" задача))
  (restart-case
      (let* ((сырой (запросить-модель задача))
             (код (извлечь-код сырой))
             (итог (выполнить-питон код)))
        (when (find-package :трас)
          (funcall (intern "ЗАПИСАТЬ-ШАГ" :трас) 1 задача код :качество :достаточно)
          (funcall (intern "ЗАПИСАТЬ-ШАГ" :трас) 2 код итог :качество :достаточно)
          (funcall (intern "ЗАВЕРШИТЬ-СЛЕД" :трас) итог))
        итог)
    (вернуть-ошибку (е)
      :report "Вернуть сообщение об ошибке"
      (let ((с (format nil "Сбой потока: ~A" е)))
        (when (find-package :трас)
          (funcall (intern "ЗАВЕРШИТЬ-СЛЕД" :трас) с))
        с))))

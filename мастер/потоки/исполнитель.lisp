;;; -*- Mode: Lisp; Coding: utf-8 -*-
;;; поток-исполнитель.lisp — принять задачу, спросить модель, выполнить код

(defpackage :поток-исполнитель
  (:use :cl)
  (:export #:выполнить))

(in-package :поток-исполнитель)

(ql:quickload '("dexador" "cl-json") :silent t)

;;; --- запрос к модели ---

(defun ключ-api ()
  (or (sb-ext:posix-getenv "OPENROUTER_API_KEY")
      (error "Нет OPENROUTER_API_KEY")))

(defun запросить-модель (задача)
  (let* ((тело (cl-json:encode-json-to-string
                `((:model . "openai/gpt-4o-mini")
                  (:messages
                   . (,(list (cons :role "system")
                             (cons :content "Верни ТОЛЬКО код Python без пояснений и markdown."))
                      ,(list (cons :role "user")
                             (cons :content (format nil "~A" задача))))))))
         (сырой (dexador:post
                 "https://openrouter.ai/api/v1/chat/completions"
                 :content тело
                 :headers `(("Content-Type" . "application/json")
                            ("Authorization" . ,(format nil "Bearer ~A" (ключ-api))))))
         (текст (if (stringp сырой) сырой
                    (sb-ext:octets-to-string сырой :external-format :utf-8)))
         (json  (cl-json:decode-json-from-string текст))
         (выбор (car (cdr (assoc :choices json))))
         (сообщ (cdr (assoc :message выбор))))
    (or (cdr (assoc :content сообщ))
        (error "Пустой ответ от модели: ~a" текст))))

;;; --- очистка кода ---

(defun убрать-обёртку (строки)
  (cond ((null строки) nil)
        ((and (>= (length (car строки)) 3)
              (string= "```" (car строки) :end2 3))
         (убрать-обёртку (cdr строки)))
        (t (cons (car строки)
                 (убрать-обёртку (cdr строки))))))

(defun очистить-код (код)
  (let ((строки (loop for s = 0 then (1+ e)
                      for e = (position #\Newline код :start s)
                      collect (subseq код s (or e (length код)))
                      while e)))
    (format nil "~{~a~%~}" (убрать-обёртку строки))))

;;; --- выполнение Python ---

(defun исполнить-python (код)
  (handler-case
      (multiple-value-bind (вывод ошибки код-вых)
          (uiop:run-program (list "python3" "-c" код)
                            :output :string
                            :error-output :string
                            :ignore-error-status t)
        (declare (ignore код-вых))
        (if (string/= ошибки "")
            (format nil "Ошибка Python: ~a" ошибки)
            (string-trim '(#\Space #\Newline) вывод)))
    (error (e) (format nil "Ошибка запуска: ~a" e))))

;;; --- точка входа ---

(defun выполнить (задача)
  (when (find-package :трас)
    (funcall (intern "НАЧАТЬ-СЛЕД" :трас) "исполнитель" задача))
  (handler-case
      (let* ((код-python (запросить-модель задача))
             (чистый-код (очистить-код код-python))
             (итог       (исполнить-python чистый-код)))
        (when (find-package :трас)
          (funcall (intern "ЗАПИСАТЬ-ШАГ" :трас) 1 задача итог :качество :достаточно)
          (funcall (intern "ЗАВЕРШИТЬ-СЛЕД" :трас) итог))
        итог)
    (error (e)
      (format nil "Ошибка исполнителя: ~a" e))))

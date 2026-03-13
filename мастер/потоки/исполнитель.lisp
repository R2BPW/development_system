;;; -*- Mode: Lisp; Coding: utf-8 -*-
;;; поток-исполнитель.lisp — принять задачу, спросить модель, выполнить код

(defpackage :поток-исполнитель
  (:use :cl)
  (:export #:выполнить))

(in-package :поток-исполнитель)

(ql:quickload '("drakma" "cl-json" "flexi-streams") :silent t)

(defun ключ-api ()
  (or (uiop:getenv "OPENROUTER_API_KEY")
      (error "Нет OPENROUTER_API_KEY")))

(defun собрать-сообщения (задача)
  (list `((:role . "system")
          (:content . "Ты генератор Python-кода. Верни ТОЛЬКО код Python без пояснений, без markdown."))
        `((:role . "user")
          (:content . ,(format nil "Напиши Python-код для: ~A" задача)))))

(defun тело-запроса (задача)
  (cl-json:encode-json-to-string
   `((:model . "openai/gpt-4o-mini")
     (:messages . ,(собрать-сообщения задача)))))

(defun запросить-модель (задача)
  (let ((ответ (drakma:http-request
                "https://openrouter.ai/api/v1/chat/completions"
                :method :post
                :content-type "application/json"
                :additional-headers `(("Authorization" . ,(format nil "Bearer ~A" (ключ-api))))
                :content (тело-запроса задача)
                :want-stream nil)))
    (извлечь-код (if (typep ответ '(vector (unsigned-byte 8)))
                     (flexi-streams:octets-to-string ответ :external-format :utf-8)
                     ответ))))

(defun извлечь-код (строка-json)
  (let* ((дерево (cl-json:decode-json-from-string строка-json))
         (выборы (cdr (assoc :choices дерево)))
         (первый (car выборы))
         (сообщение (cdr (assoc :message первый))))
    (cdr (assoc :content сообщение))))

(defun очистить-код (код)
  (let ((строки (uiop:split-string код :separator '(#\Newline))))
    (format nil "~{~A~%~}"
            (убрать-обёртку строки))))

(defun убрать-обёртку (строки)
  (cond ((null строки) nil)
        ((начинается-с "```" (car строки))
         (убрать-обёртку (cdr строки)))
        (t (cons (car строки)
                 (убрать-обёртку (cdr строки))))))

(defun начинается-с (префикс строка)
  (and (>= (length строка) (length префикс))
       (string= префикс строка :end2 (length префикс))))

(defun исполнить-python (код)
  (restart-case
      (multiple-value-bind (вывод код-ошибки статус)
          (uiop:run-program
           (list "python3" "-c" код)
           :output :string
           :error-output :string
           :ignore-error-status t)
        (declare (ignore статус))
        (if (and код-ошибки (string/= код-ошибки ""))
            (values nil код-ошибки)
            (values вывод nil)))
    (вернуть-ошибку (e)
      (values nil (format nil "Ошибка: ~A" e)))))

(defun выполнить (задача)
  (restart-case
      (let* ((код-python (запросить-модель задача))
             (чистый-код (очистить-код код-python)))
        (multiple-value-bind (результат ошибка)
            (исполнить-python чистый-код)
          (or ошибка результат)))
    (отказ (причина)
      (format nil "Отказ: ~A" причина))))

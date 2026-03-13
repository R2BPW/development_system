;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;; Имя: уточнение
;;; Описание: Итеративное решение задач — до 5 уточнений через языковую модель

(defpackage :поток-уточнение (:use :cl) (:export #:выполнить))
(in-package :поток-уточнение)

(ql:quickload '("dexador" "cl-json") :silent t)

(defun %ключ ()
  (or (sb-ext:posix-getenv "OPENROUTER_API_KEY") (error "нет OPENROUTER_API_KEY")))

(defun %сообщение (роль текст)
  `((:role . ,роль) (:content . ,текст)))

(defun %системный ()
  (%сообщение "system"
    "Решай задачу итеративно. Когда ответ полон, начни его словом «Ответ:»."))

(defun %запрос (история)
  "Отправить историю в LLM → строка-ответ."
  (handler-case
      (let* ((тело  (cl-json:encode-json-to-string
                     `((:model . "google/gemini-2.0-flash-001")
                       (:messages . ,(coerce история 'vector)))))
             (сырой (dexador:post "https://openrouter.ai/api/v1/chat/completions"
                       :content тело
                       :headers `(("Content-Type" . "application/json")
                                  ("Authorization" . ,(format nil "Bearer ~a" (%ключ))))))
             (json  (cl-json:decode-json-from-string
                     (if (stringp сырой) сырой
                         (sb-ext:octets-to-string сырой :external-format :utf-8)))))
        (cdr (assoc :content (cdr (assoc :message (first (cdr (assoc :choices json))))))))
    (error () "")))

(defun %готово-ли (ответ)
  (and (> (length ответ) 20)
       (or (search "Ответ:" ответ) (search "Итог:" ответ) (search "ГОТОВО" ответ))))

(defun %уточнить (задача предыдущий шаг)
  "Рекурсия: уточняет ответ до 5 шагов."
  (if (> шаг 5) предыдущий
      (let* ((вопрос (if (string= предыдущий "")
                         (format nil "Задача: ~a" задача)
                         (format nil "Задача: ~a~%Предыдущий ответ:~%~a~%~%Уточни." задача предыдущий)))
             (ответ  (%запрос (list (%системный) (%сообщение "user" вопрос)))))
        (if (%готово-ли ответ) ответ (%уточнить задача ответ (1+ шаг))))))

(defun выполнить (задача)
  "Точка входа. Итеративно решает задачу за 1–5 шагов."
  (handler-case (%уточнить задача "" 1)
    (error (e) (format nil "Ошибка: ~a" e))))

;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;; Имя: уточнение
;;; Описание: Итеративное решение задач — до 5 уточнений через языковую модель

(defpackage :поток-уточнение (:use :cl))
(in-package :поток-уточнение)

(ql:quickload '("drakma" "cl-json" "flexi-streams") :silent t)

(defun ключ-api ()
  "Ключ OpenRouter из окружения."
  (or (uiop:getenv "OPENROUTER_API_KEY")
      (error "Не задан OPENROUTER_API_KEY")))

(defun тело-запроса (сообщения)
  "Формирует JSON-тело для OpenRouter."
  (cl-json:encode-json-to-string
   `((:model . "google/gemini-2.0-flash-001")
     (:messages . ,(coerce сообщения 'vector)))))

(defun послать-запрос (сообщения)
  "Отправляет запрос к языковой модели, возвращает текст ответа."
  (restart-case
      (let* ((тело (тело-запроса сообщения))
             (ответ (drakma:http-request
                     "https://openrouter.ai/api/v1/chat/completions"
                     :method :post
                     :content-type "application/json"
                     :additional-headers
                     `(("Authorization" . ,(format nil "Bearer ~a" (ключ-api))))
                     :content тело
                     :want-stream nil))
             (данные (cl-json:decode-json-from-string
                      (if (stringp ответ) ответ
                          (flexi-streams:octets-to-string ответ :external-format :utf-8)))))
        (cdr (assoc :content
                    (cdr (assoc :message
                                (car (cdr (assoc :choices данные))))))))
    (вернуть-пустое () :report "Вернуть пустой ответ" "")))

(defun достаточно-ли (ответ)
  "Проверяет, содержит ли ответ явный признак завершённости."
  (and (> (length ответ) 20)
       (or (search "ГОТОВО" ответ)
           (search "Итог:" ответ)
           (search "Ответ:" ответ))))

(defun сообщение (роль текст)
  "Создаёт одно сообщение для API."
  `((:role . ,роль) (:content . ,текст)))

(defun наставление ()
  "Системное указание для модели."
  (сообщение "system"
   (format nil "~@{~a~^ ~}"
           "Ты решаешь задачу итеративно."
           "Каждый раз уточняй и улучшай предыдущий ответ."
           "Когда ответ полон, начни его словом «Ответ:».")))

(defun уточнить (задача предыдущий шаг)
  "Рекурсивно уточняет ответ. Шаг от 1 до 5."
  (if (> шаг 5)
      предыдущий
      (let* ((подсказка
               (if (string= предыдущий "")
                   (format nil "Задача: ~a" задача)
                   (format nil "Задача: ~a~%Предыдущий ответ:~%~a~%~%Уточни и улучши." задача предыдущий)))
             (история (list (наставление) (сообщение "user" подсказка)))
             (ответ (послать-запрос история)))
        (if (достаточно-ли ответ)
            ответ
            (уточнить задача ответ (1+ шаг))))))

(defun выполнить (задача)
  "Точка входа. Итеративно решает задачу за 1–5 шагов."
  (restart-case
      (уточнить задача "" 1)
    (вернуть-ошибку (e)
      :report "Вернуть описание ошибки"
      (format nil "Ошибка: ~a" e))))

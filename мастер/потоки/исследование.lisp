;;; -*- Mode: Lisp; Coding: utf-8 -*-
;;; Имя: исследование
;;; Описание: Поток-исследование — серия подзапросов сужает пространство до одного решения.
;;;           Вход: вопрос или URL. Выход: решение (текст или ссылка).

(defpackage :поток-исследование
  (:use :cl)
  (:export #:выполнить))

(in-package :поток-исследование)

(ql:quickload '("dexador" "cl-json") :silent t)

(defparameter *модель* "openai/gpt-4.1")
(defparameter *макс-шагов* 5)

(defparameter *промпт-системы*
  "Ты исследователь. Тебе дают вопрос и контекст предыдущих шагов исследования.
На каждом шаге:
1. Проанализируй что уже известно
2. Сформулируй один конкретный подзапрос для сужения пространства
3. Дай промежуточный ответ

Когда пространство сужено до одного решения, начни ответ словом «РЕШЕНИЕ:» и дай финальный ответ.
Не повторяй уже известные факты. Каждый шаг должен приближать к решению.")

(defun ключ-апи ()
  (or (sb-ext:posix-getenv "OPENROUTER_API_KEY") (error "OPENROUTER_API_KEY не задан")))

(defun %запросить (сообщения)
  (handler-case
      (let* ((тело (cl-json:encode-json-to-string
                    `((:model . ,*модель*)
                      (:messages . ,(coerce сообщения 'vector)))))
             (сырой (dexador:post "https://openrouter.ai/api/v1/chat/completions"
                       :content тело
                       :headers `(("Content-Type" . "application/json")
                                  ("Authorization" . ,(format nil "Bearer ~a" (ключ-апи))))
                       :read-timeout 90 :connect-timeout 30))
             (json (cl-json:decode-json-from-string
                    (if (stringp сырой) сырой
                        (sb-ext:octets-to-string сырой :external-format :utf-8)))))
        (or (cdr (assoc :content (cdr (assoc :message (first (cdr (assoc :choices json)))))))
            ""))
    (error (e) (format nil "Ошибка LLM: ~a" e))))

(defun %решение? (ответ)
  "Проверить наличие маркера финального решения."
  (and (> (length ответ) 10)
       (search "РЕШЕНИЕ:" ответ)))

(defun %извлечь-решение (ответ)
  "Извлечь текст после РЕШЕНИЕ:."
  (let ((поз (search "РЕШЕНИЕ:" ответ)))
    (if поз
        (string-trim '(#\Space #\Newline) (subseq ответ (+ поз 8)))
        ответ)))

(defun %поиск-ddg (запрос)
  "Поиск через DuckDuckGo Instant Answer → строка или nil."
  (handler-case
      (let* ((url (format nil "https://api.duckduckgo.com/?q=~a&format=json&no_html=1"
                          (dexador:url-encode запрос)))
             (сырой (dexador:get url :headers '(("Accept" . "application/json"))))
             (json (cl-json:decode-json-from-string
                    (if (stringp сырой) сырой
                        (sb-ext:octets-to-string сырой :external-format :utf-8))))
             (абстракт (cdr (assoc :abstract json)))
             (ответ (cdr (assoc :answer json))))
        (cond
          ((and ответ (> (length ответ) 0)) ответ)
          ((and абстракт (> (length абстракт) 0)) абстракт)
          (t nil)))
    (error () nil)))

(defun %шаг-исследования (вопрос история шаг)
  "Рекурсивный шаг. Сообщения накапливаются."
  (if (> шаг *макс-шагов*)
      (format nil "Исследование не завершено за ~a шагов.~%Последний контекст:~%~a"
              *макс-шагов* (cdr (assoc :content (car (last история)))))
      (let* ((поиск (%поиск-ddg вопрос))
             (контекст-поиска (if поиск
                                  (format nil "~%Результат поиска: ~a" поиск)
                                  ""))
             (запрос (format nil "Шаг ~a/~a. Вопрос: ~a~a"
                            шаг *макс-шагов* вопрос контекст-поиска))
             (новые-сообщения (append история
                                      (list `((:role . "user") (:content . ,запрос)))))
             (ответ (%запросить новые-сообщения)))
        (if (%решение? ответ)
            (%извлечь-решение ответ)
            (%шаг-исследования
             ответ
             (append новые-сообщения
                     (list `((:role . "assistant") (:content . ,ответ))))
             (1+ шаг))))))

(defun выполнить (задача)
  "Точка входа. Итеративное исследование: вопрос → серия подзапросов → решение."
  (handler-case
      (let ((начальная-история
             (list `((:role . "system") (:content . ,*промпт-системы*)))))
        (%шаг-исследования задача начальная-история 1))
    (error (e) (format nil "Ошибка исследования: ~a" e))))

;;; -*- Mode: Lisp; Coding: utf-8 -*-
;;; Имя: план
;;; Описание: Поток-порождатель — анализирует проблему, генерирует .md файл
;;;           с задачами для автономного исполнения. Каждый план — порождатель потоков.

(defpackage :поток-план
  (:use :cl)
  (:export #:выполнить))

(in-package :поток-план)

(ql:quickload '("dexador" "cl-json") :silent t)

(defparameter *каталог-планов*
  (or (sb-ext:posix-getenv "PLANS_DIR") "/tmp/планы/"))

(defparameter *модель* "openai/gpt-4.1")

(defparameter *промпт-системы*
  "Ты планировщик задач. Генерируй план в формате Markdown.
Структура:
# <название плана>
## Контекст
<краткое описание проблемы>
## Задачи
Каждая задача — отдельный пункт:
### Задача N: <название>
- **Тип потока**: <артефакт|исследование|верификатор|исполнитель>
- **Вход**: <что подать на вход потоку>
- **Критерий готовности**: <когда задача считается выполненной>
- **Зависимости**: <от каких задач зависит, или «нет»>
## Порядок выполнения
<последовательность или параллельные группы>
## Риски
<что может пойти не так>

Будь конкретен. Не пиши абстрактных задач вроде «улучшить качество». Каждая задача — вызываемый поток.")

(defun ключ-апи ()
  (or (sb-ext:posix-getenv "OPENROUTER_API_KEY") (error "OPENROUTER_API_KEY не задан")))

(defun %запросить (задача)
  "Отправить задачу в LLM → строка-план."
  (handler-case
      (let* ((тело (cl-json:encode-json-to-string
                    `((:model . ,*модель*)
                      (:messages . #(((:role . "system") (:content . ,*промпт-системы*))
                                     ((:role . "user") (:content . ,задача)))))))
             (сырой (dexador:post "https://openrouter.ai/api/v1/chat/completions"
                       :content тело
                       :headers `(("Content-Type" . "application/json")
                                  ("Authorization" . ,(format nil "Bearer ~a" (ключ-апи))))
                       :read-timeout 120 :connect-timeout 30))
             (json (cl-json:decode-json-from-string
                    (if (stringp сырой) сырой
                        (sb-ext:octets-to-string сырой :external-format :utf-8)))))
        (or (cdr (assoc :content (cdr (assoc :message (first (cdr (assoc :choices json)))))))
            ""))
    (error (e) (format nil "Ошибка LLM: ~a" e))))

(defun %имя-плана (задача)
  "Извлечь имя из первых слов задачи."
  (let* ((слова (remove-if (lambda (s) (zerop (length s)))
                           (uiop:split-string задача :separator '(#\Space))))
         (имя (format nil "~{~a~^-~}" (subseq слова 0 (min 3 (length слова))))))
    (string-downcase имя)))

(defun %записать-план (имя текст)
  "Записать план в .md файл. Вернуть путь."
  (ensure-directories-exist *каталог-планов*)
  (let ((путь (format nil "~a~a.md" *каталог-планов* имя)))
    (with-open-file (f путь :direction :output :if-exists :supersede :external-format :utf-8)
      (write-string текст f))
    путь))

(defun %убрать-обёртку (текст)
  "Убрать markdown-обёртку если LLM добавил ```."
  (let* ((start (search "```" текст))
         (code-start (when start (position #\Newline текст :start start)))
         (end (when code-start (search "```" текст :start2 (1+ code-start)))))
    (if (and code-start end)
        (string-trim '(#\Newline #\Space) (subseq текст (1+ code-start) end))
        текст)))

(defun выполнить (задача)
  "Точка входа. Анализирует проблему → генерирует план в .md."
  (handler-case
      (let* ((план-текст (%убрать-обёртку (%запросить задача)))
             (имя (%имя-плана задача))
             (путь (%записать-план имя план-текст)))
        (format nil "✅ План «~a» создан: ~a" имя путь))
    (error (e) (format nil "Ошибка: ~a" e))))

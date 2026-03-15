;;; -*- Mode: Lisp; Coding: utf-8 -*-
;;; Имя: анализ-рисков
;;; Описание: Поток анализ → порождатель. Принимает описание архитектуры целиком,
;;;           выявляет точки отказа, порождает план страховок.
;;;           Цепочка: анализ → текст → план → потоки.

(defpackage :поток-анализ-рисков
  (:use :cl)
  (:export #:выполнить))

(in-package :поток-анализ-рисков)

(ql:quickload '("dexador" "cl-json") :silent t)

(defparameter *модель* "openai/gpt-4.1")

(defparameter *промпт-анализа*
  "Ты аналитик рисков. Тебе дана архитектура системы.
Задача: выявить все точки отказа.

Для каждой точки отказа:
- Компонент: <что именно может сломаться>
- Сценарий: <как это произойдёт>
- Вероятность: высокая | средняя | низкая
- Последствия: <что случится с системой>
- Обнаружение: <как узнать что сломалось>

Начни с самых критичных. Будь конкретен — абстрактные риски вроде «может упасть сервер» бесполезны.")

(defparameter *промпт-плана*
  "Ты планировщик страховок. Тебе дан анализ рисков.
Для каждого риска создай задачу-страховку в формате:

### Страховка N: <название>
- **Риск**: <какой риск покрывает>
- **Тип потока**: <аудит|исследование|артефакт|исполнитель>
- **Действие**: <конкретное действие для реализации>
- **Критерий**: <когда страховка считается внедрённой>

В конце: порядок внедрения и зависимости между страховками.")

(defun ключ-апи ()
  (or (sb-ext:posix-getenv "OPENROUTER_API_KEY") (error "OPENROUTER_API_KEY не задан")))

(defun %запросить (системный пользовательский)
  (handler-case
      (let* ((тело (cl-json:encode-json-to-string
                    `((:model . ,*модель*)
                      (:messages . #(((:role . "system") (:content . ,системный))
                                     ((:role . "user") (:content . ,пользовательский)))))))
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

(defun %собрать-контекст (задача)
  "Если задача — путь к репо, собрать структуру. Иначе — текст как есть."
  (let ((путь (string-trim '(#\Space #\Newline) задача)))
    (if (uiop:directory-exists-p путь)
        (handler-case
            (let ((дерево (uiop:run-program (list "find" путь "-name" "*.lisp" "-o" "-name" "*.scm"
                                                   "-o" "-name" "*.py" "-o" "-name" "*.sh")
                                            :output :string :error-output nil
                                            :ignore-error-status t)))
              (format nil "Репозиторий: ~a~%Файлы:~%~a~%~%Описание: ~a" путь дерево задача))
          (error () задача))
        задача)))

(defun %записать-план (анализ план)
  "Записать результат в файл."
  (let ((путь (format nil "/tmp/анализ-рисков-~a.md" (get-universal-time))))
    (ensure-directories-exist "/tmp/")
    (with-open-file (f путь :direction :output :if-exists :supersede :external-format :utf-8)
      (format f "# Анализ рисков~%~%~a~%~%# План страховок~%~%~a" анализ план))
    путь))

(defun %попробовать-породить-план (план)
  "Если доступен поток-план, делегировать ему. Иначе — nil."
  (let ((pkg (find-package :поток-план)))
    (when pkg
      (let ((fn (find-symbol "ВЫПОЛНИТЬ" pkg)))
        (when (and fn (fboundp fn))
          (handler-case (funcall (symbol-function fn) план)
            (error () nil)))))))

(defun выполнить (задача)
  "Точка входа. Анализ рисков архитектуры → план страховок.
   Вход: описание архитектуры или путь к репо.
   Выход: анализ + план. Если доступен поток-план — порождает .md."
  (handler-case
      (let* ((контекст (%собрать-контекст задача))
             (анализ (%запросить *промпт-анализа* контекст))
             (план (%запросить *промпт-плана*
                               (format nil "Анализ рисков:~%~a~%~%Исходная архитектура:~%~a"
                                       анализ контекст)))
             (путь-файла (%записать-план анализ план))
             (порождён (%попробовать-породить-план план)))
        (format nil "~a~%~%Файл: ~a~a"
                анализ путь-файла
                (if порождён (format nil "~%Порождён план: ~a" порождён) "")))
    (error (e) (format nil "Ошибка анализа рисков: ~a" e))))

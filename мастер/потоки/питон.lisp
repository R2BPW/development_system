;;; -*- Mode: Lisp -*-
;;; Имя: поток-питон
;;; Описание: принять задачу, спросить модель о Python-коде, выполнить, вернуть вывод
;;; Также экспортирует общие утилиты для других потоков-исполнителей.

(defpackage :поток-питон
  (:use :cl)
  (:export #:выполнить #:выполнить-с-моделью
           #:запросить-модель #:извлечь-код #:очистить-код #:выполнить-питон))

(in-package :поток-питон)

(ql:quickload '("dexador" "cl-json") :silent t)

(defvar *модель-по-умолчанию* "openai/gpt-4.1")

(defun ключ-апи ()
  (or (sb-ext:posix-getenv "OPENROUTER_API_KEY")
      (error "OPENROUTER_API_KEY не задан")))

(defun собрать-сообщения (задача)
  (list `((:role . "system")
          (:content . "Ты генератор Python-кода. Верни ТОЛЬКО код без пояснений, без markdown. Код выводит результат через print."))
        `((:role . "user")
          (:content . ,(format nil "Напиши Python-код для задачи: ~A" задача)))))

(defun тело-запроса (задача &optional (модель *модель-по-умолчанию*))
  (cl-json:encode-json-to-string
   `((:model . ,модель)
     (:messages . ,(собрать-сообщения задача)))))

(defun запросить-модель (задача &optional (модель *модель-по-умолчанию*))
  (let* ((сырой (dexador:post
                 "https://openrouter.ai/api/v1/chat/completions"
                 :content (тело-запроса задача модель)
                 :headers `(("Content-Type" . "application/json")
                            ("Authorization" . ,(format nil "Bearer ~A" (ключ-апи))))
                 :read-timeout 60 :connect-timeout 30)))
    (if (stringp сырой) сырой
        (sb-ext:octets-to-string сырой :external-format :utf-8))))

;;; --- извлечение и очистка кода ---

(defun и-префикс-p (префикс строка)
  (and (>= (length строка) (length префикс))
       (string= префикс (subseq строка 0 (length префикс)))))

(defun убрать-обёртку (текст)
  (let* ((поз (position #\Newline текст))
         (без-начала (if поз (subseq текст (1+ поз)) текст))
         (конец (search "```" без-начала :from-end t)))
    (string-trim '(#\Space #\Newline #\Return)
                 (if конец (subseq без-начала 0 конец) без-начала))))

(defun очистить-код (текст)
  (if (null текст) ""
      (let ((ч (string-trim '(#\Space #\Newline #\Return) текст)))
        (cond ((и-префикс-p "```python" ч) (убрать-обёртку ч))
              ((и-префикс-p "```" ч) (убрать-обёртку ч))
              (t ч)))))

(defun извлечь-код (ответ-json)
  (let* ((разбор (cl-json:decode-json-from-string ответ-json))
         (выборы (cdr (assoc :choices разбор)))
         (сообщение (cdr (assoc :message (car выборы))))
         (содержимое (cdr (assoc :content сообщение))))
    (очистить-код содержимое)))

;;; --- выполнение Python ---

(defun выполнить-питон (код)
  "Записать код во временный файл и выполнить python3."
  (let ((путь (format nil "/tmp/поток-питон-~A-~A.py" (get-universal-time) (random 1000000))))
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

;;; --- общая точка входа с параметром модели ---

(defun выполнить-с-моделью (имя-потока задача модель)
  "Общий pipeline: запросить модель → извлечь код → выполнить Python."
  (when (find-package :трас)
    (funcall (intern "НАЧАТЬ-СЛЕД" :трас) имя-потока задача))
  (handler-case
      (let* ((сырой (запросить-модель задача модель))
             (код (извлечь-код сырой))
             (итог (выполнить-питон код)))
        (when (find-package :трас)
          (funcall (intern "ЗАПИСАТЬ-ШАГ" :трас) 1 задача код :качество :достаточно)
          (funcall (intern "ЗАПИСАТЬ-ШАГ" :трас) 2 код итог :качество :достаточно)
          (funcall (intern "ЗАВЕРШИТЬ-СЛЕД" :трас) итог))
        итог)
    (error (e)
      (let ((с (format nil "Сбой потока ~A: ~A" имя-потока e)))
        (when (find-package :трас)
          (funcall (intern "ЗАВЕРШИТЬ-СЛЕД" :трас) с))
        с))))

;;; --- точка входа (совместимость) ---

(defun выполнить (задача)
  (выполнить-с-моделью "поток-питон" задача *модель-по-умолчанию*))

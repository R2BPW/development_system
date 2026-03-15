;;; -*- Mode: Lisp; Coding: utf-8 -*-
;;; Имя: документация
;;; Описание: Поток-артефакт — принимает контекст проекта, генерирует LaTeX через LLM,
;;;           компилирует в PDF. Повторные вызовы мутируют тот же файл.

(defpackage :поток-документация
  (:use :cl)
  (:export #:выполнить))

(in-package :поток-документация)

(ql:quickload '("dexador" "cl-json") :silent t)

(defparameter *каталог-выхода*
  (or (sb-ext:posix-getenv "DOCS_OUTPUT_DIR") "/tmp/документация/"))

(defparameter *модель* "openai/gpt-4.1")

(defparameter *промпт-системы*
  "Ты генератор LaTeX-документации. Верни ТОЛЬКО LaTeX-код, без markdown, без ```.
Требования:
- \\documentclass[a4paper,11pt]{article}
- \\usepackage[utf8]{inputenc}, \\usepackage[T2A]{fontenc}, \\usepackage[russian]{babel}
- Структурированный документ: \\title, \\author, \\maketitle, \\tableofcontents, \\section
- Технический стиль, без воды
- Если дан существующий LaTeX и инструкция — верни мутированную версию целиком")

(defun ключ-апи ()
  (or (sb-ext:posix-getenv "OPENROUTER_API_KEY") (error "OPENROUTER_API_KEY не задан")))

(defun %запросить (сообщения)
  "Отправить сообщения в LLM → строка-ответ."
  (handler-case
      (let* ((тело (cl-json:encode-json-to-string
                    `((:model . ,*модель*)
                      (:messages . ,(coerce сообщения 'vector)))))
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

(defun %очистить-код (текст)
  "Убирает ```-обёртку если LLM добавил."
  (let* ((start (search "```" текст))
         (code-start (when start (position #\Newline текст :start start)))
         (end (when code-start (search "```" текст :start2 (1+ code-start)))))
    (if (and code-start end)
        (string-trim '(#\Newline #\Space) (subseq текст (1+ code-start) end))
        текст)))

(defun %имя-файла (задача)
  "Извлечь имя из задачи или дать дефолтное."
  (let* ((слова (remove-if (lambda (s) (zerop (length s)))
                           (uiop:split-string задача :separator '(#\Space))))
         (первое (if слова (first слова) "документ")))
    (substitute #\- #\Space (string-downcase первое))))

(defun %путь-tex (имя)
  (ensure-directories-exist *каталог-выхода*)
  (format nil "~a~a.tex" *каталог-выхода* имя))

(defun %путь-pdf (имя)
  (format nil "~a~a.pdf" *каталог-выхода* имя))

(defun %читать-существующий (путь)
  "Прочитать существующий tex если есть."
  (handler-case
      (when (probe-file путь)
        (uiop:read-file-string путь))
    (error () nil)))

(defun %записать-tex (путь код)
  (with-open-file (f путь :direction :output :if-exists :supersede :external-format :utf-8)
    (write-string код f))
  путь)

(defun %компилировать-pdf (путь-tex)
  "pdflatex дважды для оглавления. Возвращает t при успехе."
  (handler-case
      (let ((каталог (directory-namestring путь-tex)))
        (flet ((запуск ()
                 (uiop:run-program
                  (list "pdflatex" "-interaction=nonstopmode"
                        "-output-directory" каталог путь-tex)
                  :output nil :error-output nil :ignore-error-status t)))
          (запуск)
          (zerop (nth-value 2 (запуск)))))
    (error () nil)))

(defun %собрать-сообщения (задача существующий)
  "Формирует сообщения: системный + контекст + задача."
  (append
   (list `((:role . "system") (:content . ,*промпт-системы*)))
   (when существующий
     (list `((:role . "user")
             (:content . ,(format nil "Текущий документ:~%~a" существующий)))
           `((:role . "assistant") (:content . "Понял, жду инструкцию по изменению."))))
   (list `((:role . "user") (:content . ,задача)))))

(defun выполнить (задача)
  "Точка входа. Генерирует или мутирует LaTeX-документ, компилирует в PDF."
  (handler-case
      (let* ((имя (%имя-файла задача))
             (путь-tex (%путь-tex имя))
             (существующий (%читать-существующий путь-tex))
             (сообщения (%собрать-сообщения задача существующий))
             (сырой (%запросить сообщения))
             (код (%очистить-код сырой)))
        (%записать-tex путь-tex код)
        (let ((pdf? (%компилировать-pdf путь-tex)))
          (format nil "~a ~a~%Файл: ~a"
                  (if существующий "Обновлён" "Создан") имя
                  (if pdf? (%путь-pdf имя) (format nil "~a (PDF не собрался)" путь-tex)))))
    (error (e) (format nil "Ошибка: ~a" e))))

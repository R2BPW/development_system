;;; -*- Mode: Lisp; Coding: utf-8 -*-
;;; реакт.lisp — ReAct-агент: Reason + Act + Observe
;;; Инструменты: калькулятор, поиск DuckDuckGo
;;; Точка входа: (выполнить задача)

(defpackage :поток-реакт
  (:use :cl)
  (:export :выполнить))

(in-package :поток-реакт)

(ql:quickload '("dexador" "cl-json") :silent t)

;;; --- константы ---

(defparameter *модель* "google/gemini-2.0-flash-001")
(defparameter *макс-шагов* 7)
(defparameter *api-url* "https://openrouter.ai/api/v1/chat/completions")

(defparameter *системный-промпт*
  "Ты ReAct-агент. На каждом шаге думай так:
Мысль: <твои рассуждения>
Действие: поиск | калькулятор | ответ
Вход: <аргумент>

Для поиска: Действие: поиск / Вход: <запрос>
Для вычислений: Действие: калькулятор / Вход: <выражение>
Для финального ответа: Действие: ответ / Вход: <текст ответа>
Всегда начинай с Мысль:")

;;; --- URL-кодирование ---

(defun %url-encode (строка)
  "Простое percent-кодирование для UTF-8 строки."
  (with-output-to-string (out)
    (loop for октет across (sb-ext:string-to-octets строка :external-format :utf-8)
          do (if (or (<= (char-code #\A) октет (char-code #\Z))
                     (<= (char-code #\a) октет (char-code #\z))
                     (<= (char-code #\0) октет (char-code #\9))
                     (member октет (list (char-code #\-) (char-code #\_)
                                         (char-code #\.) (char-code #\~))))
                 (write-char (code-char октет) out)
                 (format out "%~2,'0X" октет)))))

;;; --- инструменты ---

(defun калькулятор (выражение)
  "Вычисляет арифметическое выражение."
  (restart-case
      (let* ((очищено (remove-if (lambda (c)
                                   (not (member c '(#\0 #\1 #\2 #\3 #\4 #\5
                                                    #\6 #\7 #\8 #\9 #\. #\+
                                                    #\- #\* #\/ #\( #\) #\space))))
                                 выражение))
             (результат (eval (read-from-string очищено))))
        (format nil "~a" результат))
    (ошибка-вычисления ()
      (format nil "Ошибка: не удалось вычислить ~s" выражение))))

(defun поиск (запрос)
  "Поиск через DuckDuckGo Instant Answer API."
  (restart-case
      (let* ((кодировка (%url-encode запрос))
             (url (format nil
                          "https://api.duckduckgo.com/?q=~a&format=json&no_html=1&skip_disambig=1"
                          кодировка))
             (сырой (dexador:get url
                                 :headers '(("Accept" . "application/json"))))
             (json (cl-json:decode-json-from-string
                    (if (stringp сырой) сырой
                        (sb-ext:octets-to-string сырой :external-format :utf-8))))
             (абстракт (cdr (assoc :abstract json)))
             (ответ-dd (cdr (assoc :answer json))))
        (cond
          ((and ответ-dd (> (length ответ-dd) 0)) ответ-dd)
          ((and абстракт (> (length абстракт) 0)) абстракт)
          (t "Результатов не найдено. Попробуй уточнить запрос.")))
    (ошибка-поиска ()
      "Ошибка при обращении к поисковику.")))

;;; --- LLM ---

(defun вызов-llm (сообщения)
  "Обращается к OpenRouter, возвращает текст ответа."
  (let* ((ключ (or (sb-ext:posix-getenv "OPENROUTER_API_KEY")
                   (error "OPENROUTER_API_KEY не задан")))
         (тело (cl-json:encode-json-to-string
                `((:model    . ,*модель*)
                  (:messages . ,сообщения))))
         (сырой (dexador:post
                 *api-url*
                 :content тело
                 :headers `(("Content-Type" . "application/json")
                            ("Authorization" . ,(format nil "Bearer ~a" ключ)))))
         (текст (if (stringp сырой) сырой
                    (sb-ext:octets-to-string сырой :external-format :utf-8)))
         (json (cl-json:decode-json-from-string текст))
         (варианты (cdr (assoc :choices json)))
         (первый   (and варианты (car варианты)))
         (сообщ    (cdr (assoc :message первый)))
         (контент  (cdr (assoc :content сообщ))))
    (or контент (error "Пустой ответ от модели: ~a" текст))))

;;; --- парсинг ответа агента ---

(defun извлечь-поле (текст метка)
  "Извлекает значение после 'Метка: ' в тексте."
  (let* ((шаблон (format nil "~a: " метка))
         (поз    (search шаблон текст)))
    (when поз
      (let* ((старт (+ поз (length шаблон)))
             (конец (or (position #\newline текст :start старт) (length текст))))
        (string-trim " " (subseq текст старт конец))))))

(defun разобрать-ответ (текст)
  "Возвращает (values действие вход)."
  (values (or (извлечь-поле текст "Действие") "ответ")
          (or (извлечь-поле текст "Вход") текст)))

;;; --- применение инструмента ---

(defun применить-инструмент (действие вход)
  (let ((д (string-downcase (string-trim " " действие))))
    (cond
      ((or (string= д "поиск") (string= д "search"))
       (поиск вход))
      ((or (string= д "калькулятор") (string= д "calc"))
       (калькулятор вход))
      (t nil))))  ; nil = финальный ответ

;;; --- главный цикл ReAct ---

(defun шаг-реакт (сообщения шаг)
  "Рекурсивный цикл: думать → действовать → наблюдать."
  (when (find-package :трас)
    (funcall (intern "ЗАПИСАТЬ-ШАГ" :трас)
             шаг (format nil "шаг ~a" шаг) "..." :качество :в-процессе))
  (let* ((ответ-текст (вызов-llm сообщения))
         (dummy (multiple-value-bind (д в) (разобрать-ответ ответ-текст)
                  (list д в)))
         (действие (first dummy))
         (вход     (second dummy))
         (наблюдение (применить-инструмент действие вход)))
    (cond
      ((null наблюдение)
       ;; Финальный ответ
       (when (find-package :трас)
         (funcall (intern "ЗАПИСАТЬ-ШАГ" :трас)
                  шаг ответ-текст вход :качество :достаточно))
       вход)
      ((>= шаг *макс-шагов*)
       (format nil "Превышен лимит шагов. Последний ответ: ~a" вход))
      (t
       ;; Добавляем наблюдение и продолжаем
       (when (find-package :трас)
         (funcall (intern "ЗАПИСАТЬ-ШАГ" :трас)
                  шаг ответ-текст наблюдение :качество :недостаточно))
       (let ((новые-сообщения
              (append сообщения
                      (list `((:role . "assistant") (:content . ,ответ-текст))
                            `((:role . "user")
                              (:content . ,(format nil "Наблюдение: ~a~%Продолжай." наблюдение)))))))
         (шаг-реакт новые-сообщения (1+ шаг)))))))

;;; --- точка входа ---

(defun выполнить (задача)
  "Запускает ReAct-агента для решения задачи."
  (when (find-package :трас)
    (funcall (intern "НАЧАТЬ-СЛЕД" :трас) "реакт" задача))
  (handler-case
      (let* ((сообщения (list `((:role . "system") (:content . ,*системный-промпт*))
                               `((:role . "user")   (:content . ,задача))))
             (итог (шаг-реакт сообщения 1)))
        (when (find-package :трас)
          (funcall (intern "ЗАВЕРШИТЬ-СЛЕД" :трас) итог))
        итог)
    (error (e)
      (format nil "Ошибка ReAct-агента: ~a" e))))

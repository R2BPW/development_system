;;;; мастер/cl/тесты.lisp
(in-package #:мастер)

;; Загружаем систему, если не в REPL
(handler-case
  (require :мастер)
  (error () nil))

(format t "[ТЕСТЫ МАСТЕР]\n")

;;; --- Тест 1: Telegram API url формата ---
(assert 
 (string= (subseq (api-url "getUpdates") 0 44)
          (concatenate 'string "https://api.telegram.org/bot" (subseq *токен* 0 1))))

;;; --- Тест 2: душа->системный-промпт на fixture ---
(let ((fixture '(:имя "Bot" :описание "Помощник" :стиль "лаконичный" :инструкции "Всегда отвечай строго по делу.")))
  (assert (search "лаконичный" (душа->системный-промпт fixture))))

;;; --- Тест 3: список-потоков возвращает .lisp ---
(let ((lst (список-потоков)))
  (assert (every (lambda (nm) (search ".lisp" nm)) lst)))

;;; --- Тест 4: /старт команда отвечает <Мастер> ---
(let ((res (обработать-команду 1 "/старт")))
  (assert (search "Мастер" res)))

(format t "Все тесты пройдены~%")

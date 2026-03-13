;;;; тесты.lisp — smoke-тесты мастер/cl
;;;; Запуск: sbcl --load ~/quicklisp/setup.lisp \
;;;;           --eval "(push #p\"мастер/cl/\" asdf:*central-registry*)" \
;;;;           --eval "(asdf:load-system :мастер :verbose nil)" \
;;;;           --load мастер/cl/тесты.lisp --eval "(sb-ext:exit)"

(in-package #:мастер)

(defvar *тест-ошибки* 0)

(defmacro тест (имя &body тело)
  `(handler-case
       (progn ,@тело (format t "  ✓ ~A~%" ,имя))
     (error (e)
       (incf *тест-ошибки*)
       (format t "  ✗ ~A: ~A~%" ,имя e))))

(format t "[ТЕСТЫ МАСТЕР]~%")

;; 1. api-url формат
(тест "api-url формат"
  (let ((url (api-url "getUpdates")))
    (assert (search "telegram.org/bot" url))
    (assert (search "getUpdates" url))))

;; 2. душа->системный-промпт
(тест "душа->системный-промпт"
  (let* ((фикстура '(:имя "Тест" :описание "тест-бот" :стиль "краткий" :инструкции "отвечай коротко"))
         (промпт (душа->системный-промпт фикстура)))
    (assert (search "Тест" промпт))
    (assert (search "краткий" промпт))))

;; 3. список-потоков — возвращает список строк
(тест "список-потоков возвращает строки"
  (let ((lst (список-потоков)))
    (assert (listp lst))
    (dolist (имя lst)
      (assert (stringp имя)))))

;; 4. обработать-команду /старт
(тест "обработать-команду /старт"
  (let ((resp (обработать-команду 0 "/старт")))
    (assert (stringp resp))
    (assert (plusp (length resp)))))

;; 5. %split-words
(тест "%split-words базовый"
  (let ((res (мастер::%split-words "/запустить поток задача")))
    (assert (equal res '("/запустить" "поток" "задача")))))

;; 6. обработать-команду неизвестная
(тест "обработать-команду неизвестная → строка"
  (let ((resp (обработать-команду 0 "/хрень")))
    (assert (stringp resp))))

(format t "~%Итого ошибок: ~A~%" *тест-ошибки*)
(when (plusp *тест-ошибки*) (sb-ext:exit :code 1))

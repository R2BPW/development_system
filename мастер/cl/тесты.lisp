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

;; 2. душа->системный-промпт — новый формат (душа (ключ значение) ...)
(тест "душа->системный-промпт"
  (let* ((фикстура '(душа (указание "Тест-бот") (девиз "краткий")))
         (промпт (душа->системный-промпт фикстура)))
    (assert (search "Тест-бот" промпт))
    (assert (search "краткий" промпт))))

;; 3. %душа-значение — извлечение из структуры
(тест "%душа-значение"
  (let ((д '(душа (указание "инструкция") (модель "gpt-4") (теплота 0.5))))
    (assert (string= (%душа-значение д 'указание) "инструкция"))
    (assert (string= (%душа-значение д 'модель) "gpt-4"))
    (assert (null (%душа-значение д 'несуществующий)))))

;; 4. читать-душу — валидация формата
(тест "читать-душу валидация"
  (let ((tmp "/tmp/test-dusha-bad.scm"))
    (with-open-file (out tmp :direction :output :if-exists :supersede)
      (prin1 '(не-душа (a 1)) out))
    (assert (null (ignore-errors (читать-душу tmp))))))

;; 5. список-потоков — возвращает список строк
(тест "список-потоков возвращает строки"
  (let ((lst (список-потоков)))
    (assert (listp lst))
    (dolist (имя lst)
      (assert (stringp имя)))))

;; 6. обработать-команду /старт → строка (не nil)
(тест "обработать-команду /старт → строка"
  (let ((resp (обработать-команду 0 "/старт")))
    (assert (stringp resp))
    (assert (plusp (length resp)))))

;; 7. %обр-старт возвращает (values строка markup)
(тест "%обр-старт возвращает values"
  (multiple-value-bind (текст markup) (%обр-старт 0 nil)
    (assert (stringp текст))
    (assert (plusp (length текст)))
    (assert markup)))

;; 8. %split-words
(тест "%split-words базовый"
  (let ((res (%split-words "/запустить поток задача")))
    (assert (equal res '("/запустить" "поток" "задача")))))

(тест "%split-words пустая строка"
  (assert (null (%split-words ""))))

;; 9. %llm-key ленивое чтение
(тест "%llm-key ленивое"
  (let ((*llm-key* "test-key"))
    (assert (string= (%llm-key) "test-key"))))

;; 10. обработать-команду неизвестная
(тест "обработать-команду неизвестная → строка"
  (let ((resp (обработать-команду 0 "/хрень")))
    (assert (stringp resp))))

(format t "~%Итого ошибок: ~A~%" *тест-ошибки*)
(when (plusp *тест-ошибки*) (sb-ext:exit :code 1))

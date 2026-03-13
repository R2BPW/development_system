#lang racket/base

;;; unit.rkt — юнит-тесты чистых функций (без сети, без SBCL)

(require rackunit
         rackunit/text-ui
         racket/file
         racket/list
         racket/path
         racket/string
         (file "../мастер/potoki.rkt")
         (file "../мастер/dusha.rkt"))

;; ============================================================
;; извлечь-код
;; ============================================================

(define suite-извлечь-код
  (test-suite "извлечь-код"

    (test-case "пустой ответ → пустая строка"
      (check-equal? (string-trim (извлечь-код "")) ""))

    (test-case "голый код без обёртки"
      (define код "(defun f () 42)")
      (check-equal? (string-trim (извлечь-код код)) код))

    (test-case "блок ```lisp"
      (define вход "```lisp\n(defun f () 42)\n```")
      (check-equal? (string-trim (извлечь-код вход)) "(defun f () 42)"))

    (test-case "блок ``` без языка"
      (define вход "Вот код:\n```\n(defun f () 42)\n```\nГотово.")
      (check-equal? (string-trim (извлечь-код вход)) "(defun f () 42)"))

    (test-case "блок ```common-lisp"
      (define вход "```common-lisp\n(defpackage :тест (:use :cl))\n```")
      (check-true (string-contains? (извлечь-код вход) "defpackage")))

    (test-case "мусор до и после блока"
      (define вход "Объяснение...\n```\n(+ 1 2)\n```\nПодпись.")
      (check-equal? (string-trim (извлечь-код вход)) "(+ 1 2)"))

    (test-case "несколько строк кода"
      (define вход "```\n(defun f (x)\n  (* x 2))\n```")
      (check-true (string-contains? (извлечь-код вход) "defun"))
      (check-true (string-contains? (извлечь-код вход) "(* x 2)")))))

;; ============================================================
;; имя-пакета
;; ============================================================

(define suite-имя-пакета
  (test-suite "имя-пакета"

    (test-case "простое имя"
      (define код "(defpackage :поток-уточнение (:use :cl))")
      (check-equal? (имя-пакета код) "уточнение"))

    (test-case "реакт"
      (define код "(defpackage :поток-реакт (:use :cl))")
      (check-equal? (имя-пакета код) "реакт"))

    (test-case "латинское имя"
      (define код "(defpackage :поток-python-exec (:use :cl))")
      (check-equal? (имя-пакета код) "python-exec"))

    (test-case "нет пакета → безымянный"
      (check-equal? (имя-пакета "(+ 1 2)") "безымянный"))

    (test-case "бэктик не захватывается"
      (define код ":поток-питон`")
      (check-equal? (имя-пакета код) "питон"))))

;; ============================================================
;; dusha.rkt — прочитать-душу / получить-поле / заменить-поле
;; ============================================================

(define тест-конфиг
  '(душа
    (указание  "системный промпт")
    (модель    "test-model")
    (теплота   0.5)
    (память    ((ключ . "значение")))
    (девиз     "тест")))

(define suite-dusha
  (test-suite "dusha"

    (test-case "получить-поле: указание"
      (check-equal? (получить-поле тест-конфиг 'указание) "системный промпт"))

    (test-case "получить-поле: теплота"
      (check-equal? (получить-поле тест-конфиг 'теплота) 0.5))

    (test-case "получить-поле: несуществующее поле → #f"
      (check-false (получить-поле тест-конфиг 'несуществует)))

    (test-case "заменить-поле: изменяет значение"
      (define новый (заменить-поле тест-конфиг 'модель "new-model"))
      (check-equal? (получить-поле новый 'модель) "new-model"))

    (test-case "заменить-поле: не мутирует оригинал"
      (define _ (заменить-поле тест-конфиг 'модель "другая"))
      (check-equal? (получить-поле тест-конфиг 'модель) "test-model"))

    (test-case "прочитать-душу: читает файл"
      (define путь (make-temporary-file))
      (call-with-output-file путь
        (lambda (p)
          (write '(душа (модель "file-model") (теплота 0.3)) p))
        #:exists 'replace)
      (define д (прочитать-душу путь))
      (check-equal? (получить-поле д 'модель) "file-model")
      (delete-file путь))

    (test-case "прочитать-душу: несуществующий файл → #f"
      (check-false (прочитать-душу "/нет/такого/файла.scm")))))

;; ============================================================
;; список-потоков / прочитать-активный
;; ============================================================

(define suite-файловая-система
  (test-suite "файловая система потоков"

    (test-case "записать-поток создаёт файл"
      (define вр (make-temporary-file "тест~a" 'directory))
      (define код "(defpackage :поток-тест (:use :cl))\n(in-package :поток-тест)")
      (define путь (записать-поток вр "тест" код))
      (check-true (file-exists? путь))
      (check-true (string-contains? (file->string путь) "defpackage"))
      (delete-directory/files вр))

    (test-case "записать-мета создаёт .мета файл"
      (define вр (make-temporary-file "тест~a" 'directory))
      (define путь (записать-мета вр "тест" "описание теста"))
      (check-true (file-exists? путь))
      (define мета (call-with-input-file путь read))
      (check-equal? (поле-меты мета 'имя) "тест")
      (check-equal? (поле-меты мета 'описание) "описание теста")
      (delete-directory/files вр))

    (test-case "список-потоков: пустой каталог"
      (define вр (make-temporary-file "тест~a" 'directory))
      (check-equal? (список-потоков вр) '())
      (delete-directory/files вр))

    (test-case "список-потоков: находит потоки"
      (define вр (make-temporary-file "тест~a" 'directory))
      (записать-поток вр "альфа" "(defpackage :поток-альфа (:use :cl))")
      (записать-мета  вр "альфа" "первый поток")
      (записать-поток вр "бета"  "(defpackage :поток-бета  (:use :cl))")
      (записать-мета  вр "бета"  "второй поток")
      (define потоки (список-потоков вр))
      (check-equal? (length потоки) 2)
      (delete-directory/files вр))))

;; ============================================================
;; Запуск
;; ============================================================

(define all-suites
  (list suite-извлечь-код
        suite-имя-пакета
        suite-dusha
        suite-файловая-система))

(define (запустить-тесты!)
  (for/fold ([итог 0]) ([suite (in-list all-suites)])
    (+ итог (run-tests suite))))

(module+ main
  (define провалов (запустить-тесты!))
  (exit (if (zero? провалов) 0 1)))

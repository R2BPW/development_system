#lang racket/base

;;; Тесты для модуля потоков — порождение, запись, проверка

(require rackunit
         racket/file
         racket/list
         racket/path
         racket/string
         racket/system
         "potoki.rkt"
         "dusha.rkt")

;; --- вспомогательные ---

(define тест-душа
  '(душа
    (указание "Ты помощник для создания вычислительных потоков.")
    (модель "claude-sonnet-4-20250514")
    (теплота 0.3)
    (память ())
    (девиз "краткость, однострочники, без англицизмов, SICP")))

(define тест-код
  (string-append
   ";;; -*- Mode: Lisp; Syntax: Common-Lisp -*-\n"
   ";;; Имя: сумма\n"
   ";;; Описание: Складывает числа\n"
   "\n"
   "(defpackage :поток-сумма (:use :cl))\n"
   "(in-package :поток-сумма)\n"
   "\n"
   "(defun выполнить (задача)\n"
   "  \"Точка входа.\"\n"
   "  (format nil \"Сумма: ~a\" задача))\n"))

;; --- тесты формирования задания ---

(test-case "сформировать-задание: содержит описание и девиз"
  (let ((задание (сформировать-задание тест-душа "сложить числа")))
    (check-true (string-contains? задание "сложить числа"))
    (check-true (string-contains? задание "краткость"))
    (check-true (string-contains? задание "вычислительных потоков"))
    (check-true (string-contains? задание "defun выполнить"))))

(test-case "сформировать-задание: содержит указание из души"
  (let ((задание (сформировать-задание тест-душа "задача")))
    (check-true (string-contains? задание "помощник для создания"))))

;; --- тесты извлечения кода ---

(test-case "извлечь-код: код в блоке ```lisp"
  (let ((ответ "Вот код:\n```lisp\n(defun f () 42)\n```\nГотово."))
    (check-equal? (извлечь-код ответ) "(defun f () 42)")))

(test-case "извлечь-код: код в блоке ```common-lisp"
  (let ((ответ "```common-lisp\n(+ 1 2)\n```"))
    (check-equal? (извлечь-код ответ) "(+ 1 2)")))

(test-case "извлечь-код: голый код без блока"
  (let ((ответ "(defpackage :поток-тест (:use :cl))"))
    (check-equal? (извлечь-код ответ) ответ)))

(test-case "извлечь-код: многострочный блок"
  (let ((ответ "```lisp\n(defpackage :поток-x (:use :cl))\n(in-package :поток-x)\n```"))
    (check-true (string-contains? (извлечь-код ответ) "defpackage"))))

;; --- тесты имени пакета ---

(test-case "имя-пакета: находит имя"
  (check-equal? (имя-пакета "(defpackage :поток-сумма (:use :cl))") "сумма"))

(test-case "имя-пакета: безымянный при отсутствии"
  (check-equal? (имя-пакета "(defun f () 42)") "безымянный"))

;; --- тесты записи файлов ---

(test-case "записать-поток: создаёт файл"
  (let ((кат (make-temporary-file "потоки-~a" 'directory)))
    (let ((путь (записать-поток кат "тест" тест-код)))
      (check-true (file-exists? путь))
      (check-true (string-contains? (file->string путь) "defpackage")))
    (delete-directory/files кат)))

(test-case "записать-мета: создаёт файл с полями"
  (let ((кат (make-temporary-file "мета-~a" 'directory)))
    (let ((путь (записать-мета кат "тест" "Тестовый поток")))
      (check-true (file-exists? путь))
      (let ((содержимое (file->string путь)))
        (check-true (string-contains? содержимое "тест"))
        (check-true (string-contains? содержимое "Тестовый поток"))
        (check-true (string-contains? содержимое ":готов"))))
    (delete-directory/files кат)))

;; --- тесты проверки потока ---

(test-case "проверить-поток: корректный код"
  (let ((кат (make-temporary-file "проверка-~a" 'directory)))
    (let ((путь (записать-поток кат "проверка" тест-код)))
      (let-values ([(успех? вывод) (проверить-поток путь)])
        (check-true успех?)))
    (delete-directory/files кат)))

(test-case "проверить-поток: некорректный код"
  (let ((кат (make-temporary-file "проверка-~a" 'directory)))
    (let ((путь (записать-поток кат "плохой" "(defun незакрытая-скобка")))
      (let-values ([(успех? вывод) (проверить-поток путь)])
        (check-false успех?)))
    (delete-directory/files кат)))

;; --- тесты чтения метаданных ---

(test-case "прочитать-мету: существующий файл"
  (let ((кат (make-temporary-file "мета-~a" 'directory)))
    (let ((путь (записать-мета кат "тест" "Тестовый поток")))
      (let ((мета (прочитать-мету путь)))
        (check-true (list? мета))
        (check-equal? (поле-меты мета 'имя) "тест")
        (check-equal? (поле-меты мета 'описание) "Тестовый поток")))
    (delete-directory/files кат)))

(test-case "прочитать-мету: несуществующий файл"
  (check-false (прочитать-мету "/tmp/нет-такого-файла.мета")))

(test-case "поле-меты: состояние"
  (let ((кат (make-temporary-file "мета-~a" 'directory)))
    (let* ((путь (записать-мета кат "тест" "описание"))
           (мета (прочитать-мету путь)))
      (check-equal? (поле-меты мета 'состояние) ':готов))
    (delete-directory/files кат)))

;; --- тесты списка потоков ---

(test-case "список-потоков: пустой каталог"
  (let ((кат (make-temporary-file "потоки-~a" 'directory)))
    (check-equal? (список-потоков кат) '())
    (delete-directory/files кат)))

(test-case "список-потоков: несуществующий каталог"
  (check-equal? (список-потоков "/tmp/нет-каталога-12345") '()))

(test-case "список-потоков: каталог с потоками"
  (let ((кат (make-temporary-file "потоки-~a" 'directory)))
    (записать-поток кат "альфа" тест-код)
    (записать-мета кат "альфа" "Первый поток")
    (записать-поток кат "бета" тест-код)
    (записать-мета кат "бета" "Второй поток")
    (let ((потоки (список-потоков кат)))
      (check-equal? (length потоки) 2)
      (check-not-false (member "альфа" (map first потоки)))
      (check-not-false (member "бета" (map first потоки))))
    (delete-directory/files кат)))

;; --- тесты поиска потока ---

(test-case "найти-поток: существующий"
  (let ((кат (make-temporary-file "потоки-~a" 'directory)))
    (записать-поток кат "гамма" тест-код)
    (записать-мета кат "гамма" "Третий")
    (let ((результат (найти-поток кат "гамма")))
      (check-true (list? результат))
      (check-true (file-exists? (first результат))))
    (delete-directory/files кат)))

(test-case "найти-поток: несуществующий"
  (let ((кат (make-temporary-file "потоки-~a" 'directory)))
    (check-false (найти-поток кат "нет-такого"))
    (delete-directory/files кат)))

;; --- тесты активного потока ---

(test-case "записать-активный/прочитать-активный: цикл"
  (let ((кат (make-temporary-file "потоки-~a" 'directory)))
    (записать-поток кат "дельта" тест-код)
    (записать-активный кат "дельта" "поток-дельта")
    (let ((актив (прочитать-активный кат)))
      (check-true (list? актив))
      (check-equal? (first актив) "дельта")
      (check-equal? (third актив) "поток-дельта"))
    (delete-directory/files кат)))

(test-case "прочитать-активный: нет файла"
  (let ((кат (make-temporary-file "потоки-~a" 'directory)))
    (check-false (прочитать-активный кат))
    (delete-directory/files кат)))

(test-case "удалить-активный: удаляет файл"
  (let ((кат (make-temporary-file "потоки-~a" 'directory)))
    (записать-активный кат "эпсилон" "поток-эпсилон")
    (check-true (list? (прочитать-активный кат)))
    (удалить-активный кат)
    (check-false (прочитать-активный кат))
    (delete-directory/files кат)))

(displayln "Все тесты потоков пройдены.")

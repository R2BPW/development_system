#lang racket/base

;;; Тесты для модуля комбинирования — цепочки, параллели, ветвления

(require rackunit
         racket/file
         racket/list
         racket/path
         racket/string
         racket/system
         "kombinirovanie.rkt"
         "potoki.rkt"
         "dusha.rkt")

;; --- вспомогательные ---

(define тест-код-эхо
  (string-append
   "(defpackage :поток-эхо (:use :cl))\n"
   "(in-package :поток-эхо)\n"
   "(defun выполнить (задача)\n"
   "  (format nil \"Эхо: ~a\" задача))\n"))

(define тест-код-обёртка
  (string-append
   "(defpackage :поток-обёртка (:use :cl))\n"
   "(in-package :поток-обёртка)\n"
   "(defun выполнить (задача)\n"
   "  (format nil \"[~a]\" задача))\n"))

(define (создать-тест-каталог)
  (let ((кат (make-temporary-file "комб-~a" 'directory)))
    (записать-поток кат "эхо" тест-код-эхо)
    (записать-мета кат "эхо" "Эхо-поток")
    (записать-поток кат "обёртка" тест-код-обёртка)
    (записать-мета кат "обёртка" "Оборачивает в скобки")
    кат))

;; --- тесты сбора путей ---

(test-case "собрать-пути-потоков: существующие потоки"
  (let ((кат (создать-тест-каталог)))
    (let ((пары (собрать-пути-потоков кат '("эхо" "обёртка"))))
      (check-equal? (length пары) 2)
      (check-equal? (car (first пары)) "эхо")
      (check-equal? (car (second пары)) "обёртка")
      (check-true (string-contains? (cdr (first пары)) "эхо.lisp")))
    (delete-directory/files кат)))

(test-case "собрать-пути-потоков: несуществующий поток"
  (let ((кат (создать-тест-каталог)))
    (check-exn exn:fail?
               (lambda () (собрать-пути-потоков кат '("эхо" "нет-такого"))))
    (delete-directory/files кат)))

;; --- тесты шаблонов ---

(test-case "шаблон-цепочки: генерирует корректный код"
  (let ((код (шаблон-цепочки "тест-цепь" '(("эхо" . "/tmp/эхо.lisp")
                                             ("обёртка" . "/tmp/обёртка.lisp")))))
    (check-true (string-contains? код "поток-тест-цепь"))
    (check-true (string-contains? код "load"))
    (check-true (string-contains? код "ВЫПОЛНИТЬ"))
    (check-true (string-contains? код "defun выполнить"))
    (check-true (string-contains? код "Цепочка"))))

(test-case "шаблон-параллели: генерирует корректный код"
  (let ((код (шаблон-параллели "тест-пар" '(("эхо" . "/tmp/эхо.lisp")
                                              ("обёртка" . "/tmp/обёртка.lisp")))))
    (check-true (string-contains? код "поток-тест-пар"))
    (check-true (string-contains? код "load"))
    (check-true (string-contains? код "р0"))
    (check-true (string-contains? код "р1"))
    (check-true (string-contains? код "Параллель"))))

(test-case "шаблон-ветвления: генерирует корректный код"
  (let ((код (шаблон-ветвления "тест-ветв"
                                '(("привет" "эхо" "/tmp/эхо.lisp")
                                  ("оберни" "обёртка" "/tmp/обёртка.lisp")))))
    (check-true (string-contains? код "поток-тест-ветв"))
    (check-true (string-contains? код "search"))
    (check-true (string-contains? код "привет"))
    (check-true (string-contains? код "оберни"))
    (check-true (string-contains? код "cond"))))

;; --- тесты создания комбинированных потоков ---

(test-case "создать-цепочку: создаёт файлы"
  (let ((кат (создать-тест-каталог)))
    (let ((результат (создать-цепочку кат '("эхо" "обёртка"))))
      (check-equal? (first результат) "цепочка-эхо-обёртка")
      (check-true (file-exists? (second результат)))
      (check-true (string-contains? (third результат) "Цепочка")))
    (delete-directory/files кат)))

(test-case "создать-параллель: создаёт файлы"
  (let ((кат (создать-тест-каталог)))
    (let ((результат (создать-параллель кат '("эхо" "обёртка"))))
      (check-equal? (first результат) "параллель-эхо-обёртка")
      (check-true (file-exists? (second результат)))
      (check-true (string-contains? (third результат) "Параллель")))
    (delete-directory/files кат)))

(test-case "создать-ветвление: создаёт файлы"
  (let ((кат (создать-тест-каталог)))
    (let ((результат (создать-ветвление кат '(("привет" "эхо")
                                               ("оберни" "обёртка")))))
      (check-true (string-contains? (first результат) "ветвление"))
      (check-true (file-exists? (second результат)))
      (check-true (string-contains? (third результат) "Ветвление")))
    (delete-directory/files кат)))

;; --- тесты компиляции в SBCL ---

(test-case "создать-цепочку: код проходит проверку SBCL"
  (let ((кат (создать-тест-каталог)))
    (let* ((результат (создать-цепочку кат '("эхо" "обёртка")))
           (путь (second результат)))
      (let-values ([(успех? вывод) (проверить-поток путь)])
        (check-true успех?)))
    (delete-directory/files кат)))

(test-case "создать-параллель: код проходит проверку SBCL"
  (let ((кат (создать-тест-каталог)))
    (let* ((результат (создать-параллель кат '("эхо" "обёртка")))
           (путь (second результат)))
      (let-values ([(успех? вывод) (проверить-поток путь)])
        (check-true успех?)))
    (delete-directory/files кат)))

(test-case "создать-ветвление: код проходит проверку SBCL"
  (let ((кат (создать-тест-каталог)))
    (let* ((результат (создать-ветвление кат '(("привет" "эхо")
                                                ("оберни" "обёртка"))))
           (путь (second результат)))
      (let-values ([(успех? вывод) (проверить-поток путь)])
        (check-true успех?)))
    (delete-directory/files кат)))

;; --- тесты создания с несуществующим потоком ---

(test-case "создать-цепочку: ошибка при несуществующем потоке"
  (let ((кат (создать-тест-каталог)))
    (check-exn exn:fail?
               (lambda () (создать-цепочку кат '("эхо" "нет-такого"))))
    (delete-directory/files кат)))

;; --- тесты комбинации в метаданных ---

(test-case "создать-цепочку: записывает мету"
  (let ((кат (создать-тест-каталог)))
    (создать-цепочку кат '("эхо" "обёртка"))
    (let ((потоки (список-потоков кат)))
      (check-not-false
       (member "цепочка-эхо-обёртка" (map first потоки))))
    (delete-directory/files кат)))

(displayln "Все тесты комбинирования пройдены.")

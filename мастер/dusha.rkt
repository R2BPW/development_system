#lang racket/base

;;; Душа — чтение, запись, отображение конфигурации и истории

(require racket/file
         racket/format
         racket/list
         racket/match
         racket/path
         racket/string)

(provide прочитать-душу
         записать-душу
         получить-поле
         заменить-поле
         показать-душу
         показать-память
         добавить-память
         записать-историю
         прочитать-историю
         дата-сегодня)

;; --- чтение/запись души ---

(define (прочитать-душу путь)
  (if (file-exists? путь)
      (with-handlers ([exn:fail? (lambda (_) #f)])
        (call-with-input-file путь read))
      #f))

(define (записать-душу путь данные)
  (call-with-output-file путь
    (lambda (порт)
      (красиво-записать данные порт 0))
    #:exists 'replace))

;; --- доступ к полям ---

(define (получить-поле душа имя)
  (match душа
    [(list 'душа поля ...)
     (for/or ([поле (in-list поля)])
       (match поле
         [(list (== имя) значение) значение]
         [_ #f]))]
    [_ #f]))

(define (заменить-поле душа имя значение)
  (match душа
    [(list 'душа поля ...)
     (cons 'душа
           (for/list ([поле (in-list поля)])
             (match поле
               [(list (== имя) _) (list имя значение)]
               [другое другое])))]))

;; --- отображение ---

(define (показать-душу душа)
  (match душа
    [(list 'душа поля ...)
     (string-join
      (for/list ([поле (in-list поля)])
        (match поле
          [(list имя значение)
           (format "~a: ~a" имя значение)]))
      "\n")]
    [_ "Душа не найдена."]))

(define (показать-память душа)
  (let ((п (получить-поле душа 'память)))
    (cond
      [(or (not п) (null? п)) "Память пуста."]
      [else
       (string-join
        (for/list ([факт (in-list п)])
          (match факт
            [(cons ключ значение)
             (format "- ~a: ~a" ключ значение)]
            [другое (format "- ~a" другое)]))
        "\n")])))

(define (добавить-память душа факт)
  (let ((п (or (получить-поле душа 'память) '())))
    (заменить-поле душа 'память (append п (list факт)))))

;; --- красивая запись S-выражений ---

(define (красиво-записать данные порт отступ)
  (match данные
    [(list 'душа поля ...)
     (display "(душа" порт)
     (for ([поле (in-list поля)])
       (newline порт)
       (display "  " порт)
       (write поле порт))
     (display ")" порт)
     (newline порт)]
    [_ (write данные порт)
       (newline порт)]))

;; --- история диалогов ---

(define (дата-сегодня)
  (let ((с (seconds->date (current-seconds))))
    (format "~a-~a-~a"
            (date-year с)
            (~a (date-month с) #:min-width 2 #:pad-string "0" #:align 'right)
            (~a (date-day с) #:min-width 2 #:pad-string "0" #:align 'right))))

(define (путь-истории каталог дата)
  (build-path каталог (string-append дата ".txt")))

(define (записать-историю каталог ид-чата текст ответ)
  (let* ((дата (дата-сегодня))
         (файл (путь-истории каталог дата))
         (с (seconds->date (current-seconds)))
         (время (format "~a:~a:~a"
                        (~a (date-hour с) #:min-width 2 #:pad-string "0" #:align 'right)
                        (~a (date-minute с) #:min-width 2 #:pad-string "0" #:align 'right)
                        (~a (date-second с) #:min-width 2 #:pad-string "0" #:align 'right))))
    (unless (directory-exists? каталог)
      (make-directory* каталог))
    (call-with-output-file файл
      (lambda (порт)
        (fprintf порт "[~a] чат:~a~n< ~a~n> ~a~n~n" время ид-чата текст ответ))
      #:exists 'append)))

(define (прочитать-историю каталог дата)
  (let ((файл (путь-истории каталог дата)))
    (if (file-exists? файл)
        (file->string файл)
        #f)))

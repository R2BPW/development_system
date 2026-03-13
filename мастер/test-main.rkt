#lang racket/base

;;; Тесты для Мастера — разбор обновлений, формирование ответов

(require rackunit
         json
         racket/list
         "main.rkt")

;; --- тесты разбора обновлений ---

(test-case "разобрать-обновления: корректный ответ"
  (let* ((данные (jsexpr->string
                  (hash 'ok #t
                        'result (list
                                 (hash 'update_id 1
                                       'message
                                       (hash 'chat (hash 'id 42)
                                             'text "привет"))))))
         (результат (разобрать-обновления данные)))
    (check-equal? (length результат) 1)
    (check-true (hash? (car результат)))))

(test-case "разобрать-обновления: некорректный JSON"
  (check-equal? (разобрать-обновления "не json") '()))

(test-case "разобрать-обновления: ok=false"
  (let ((данные (jsexpr->string (hash 'ok #f 'result '()))))
    (check-equal? (разобрать-обновления данные) '())))

(test-case "разобрать-обновления: пустой результат"
  (let ((данные (jsexpr->string (hash 'ok #t 'result '()))))
    (check-equal? (разобрать-обновления данные) '())))

;; --- тесты извлечения сообщений ---

(test-case "извлечь-сообщения: одно сообщение"
  (let* ((обновления (list (hash 'update_id 1
                                 'message (hash 'chat (hash 'id 42)
                                                'text "тест"))))
         (сообщения (извлечь-сообщения обновления)))
    (check-equal? (length сообщения) 1)
    (check-equal? (first (car сообщения)) 1)
    (check-equal? (second (car сообщения)) 42)
    (check-equal? (third (car сообщения)) "тест")))

(test-case "извлечь-сообщения: обновление без message"
  (let ((обновления (list (hash 'update_id 1))))
    (check-equal? (извлечь-сообщения обновления) '())))

(test-case "извлечь-сообщения: несколько сообщений"
  (let* ((обновления (list (hash 'update_id 1
                                 'message (hash 'chat (hash 'id 10)
                                                'text "а"))
                           (hash 'update_id 2
                                 'message (hash 'chat (hash 'id 20)
                                                'text "б"))))
         (сообщения (извлечь-сообщения обновления)))
    (check-equal? (length сообщения) 2)
    (check-equal? (third (first сообщения)) "а")
    (check-equal? (third (second сообщения)) "б")))

;; --- тесты формирования ответа ---

(test-case "сформировать-ответ: эхо"
  (check-equal? (сформировать-ответ "привет") "Эхо: привет"))

(test-case "сформировать-ответ: /время"
  (let ((ответ (сформировать-ответ "/время")))
    (check-true (string? ответ))
    (check-true (> (string-length ответ) 10))))

(test-case "сформировать-ответ: /состояние"
  (check-equal? (сформировать-ответ "/состояние") "Каркас. Потоков нет."))

(displayln "Все тесты Мастера пройдены.")

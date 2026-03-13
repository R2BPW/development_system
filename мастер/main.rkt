#lang racket/base

;;; Мастер — бот-оркестратор
;;; Минимальный Telegram-бот: подключение к API, эхо-ответ

(require json
         net/url
         racket/format
         racket/list
         racket/match
         racket/port
         racket/string)

(provide разобрать-обновления
         извлечь-сообщения
         сформировать-ответ
         послать-сообщение
         получить-обновления
         запустить-опрос)

;; --- настройки ---

(define (ключ-бота)
  (or (getenv "MASTER_BOT_TOKEN")
      (error 'ключ-бота "Не задана переменная MASTER_BOT_TOKEN")))

(define (адрес-api метод)
  (string-append "https://api.telegram.org/bot" (ключ-бота) "/" метод))

;; --- разбор обновлений ---

(define (разобрать-обновления текст)
  (with-handlers ([exn:fail? (lambda (_) '())])
    (let ((данные (string->jsexpr текст)))
      (if (and (hash? данные)
               (hash-ref данные 'ok #f))
          (hash-ref данные 'result '())
          '()))))

(define (извлечь-сообщения обновления)
  (for/list ([обн (in-list обновления)]
             #:when (and (hash? обн)
                         (hash-has-key? обн 'message)))
    (let* ((сообщ (hash-ref обн 'message))
           (ид-обн (hash-ref обн 'update_id 0))
           (чат (hash-ref сообщ 'chat (hash)))
           (ид-чата (hash-ref чат 'id 0))
           (текст (hash-ref сообщ 'text "")))
      (list ид-обн ид-чата текст))))

;; --- формирование ответа ---

(define (сформировать-ответ текст)
  (match текст
    ["/время" (формат-время)]
    ["/состояние" "Каркас. Потоков нет."]
    [_ (string-append "Эхо: " текст)]))

(define (формат-время)
  (let ((сейчас (seconds->date (current-seconds))))
    (format "~a-~a-~a ~a:~a:~a"
            (date-year сейчас)
            (~два (date-month сейчас))
            (~два (date-day сейчас))
            (~два (date-hour сейчас))
            (~два (date-minute сейчас))
            (~два (date-second сейчас)))))

(define (~два ч)
  (~a ч #:min-width 2 #:pad-string "0" #:align 'right))

;; --- отправка ---

(define (послать-сообщение ид-чата текст)
  (let* ((тело (jsexpr->string
                (hash 'chat_id ид-чата
                      'text текст)))
         (адрес (string->url (адрес-api "sendMessage")))
         (заголовки '("Content-Type: application/json"))
         (порт (post-pure-port адрес
                               (string->bytes/utf-8 тело)
                               заголовки)))
    (begin0
      (port->string порт)
      (close-input-port порт))))

;; --- получение обновлений ---

(define (получить-обновления сдвиг)
  (let* ((адрес (string->url
                 (string-append (адрес-api "getUpdates")
                                "?timeout=30"
                                "&offset="
                                (number->string сдвиг))))
         (порт (get-pure-port адрес)))
    (begin0
      (port->string порт)
      (close-input-port порт))))

;; --- цикл опроса ---

(define (запустить-опрос)
  (displayln "Мастер запущен. Ожидаю сообщения...")
  (let цикл ((сдвиг 0))
    (with-handlers ([exn:fail:network?
                     (lambda (e)
                       (displayln (format "Ошибка сети: ~a" (exn-message e)))
                       (sleep 5)
                       (цикл сдвиг))])
      (let* ((ответ (получить-обновления сдвиг))
             (обновления (разобрать-обновления ответ))
             (сообщения (извлечь-сообщения обновления)))
        (let обр ((ост сообщения) (макс-сдвиг сдвиг))
          (if (null? ост)
              (цикл макс-сдвиг)
              (let* ((сообщ (car ост))
                     (ид-обн (first сообщ))
                     (ид-чата (second сообщ))
                     (текст (third сообщ))
                     (ответ-текст (сформировать-ответ текст)))
                (displayln (format "< ~a" текст))
                (displayln (format "> ~a" ответ-текст))
                (послать-сообщение ид-чата ответ-текст)
                (обр (cdr ост) (max макс-сдвиг (+ ид-обн 1))))))))))

;; --- точка входа ---

(module+ main
  (запустить-опрос))

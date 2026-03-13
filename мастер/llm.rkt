#lang racket/base

;;; llm.rkt — обращение к языковой модели через OpenRouter

(require json
         net/url
         racket/port
         racket/string)

(provide спросить-модель)

(define api-url "https://openrouter.ai/api/v1/chat/completions")

(define (ключ-openrouter)
  (or (getenv "OPENROUTER_KEY")
      (error 'ключ-openrouter "Не задана переменная OPENROUTER_KEY")))

;; Нормализует имя модели: claude-sonnet-4-20250514 → anthropic/claude-sonnet-4-5
(define (нормализовать-модель м)
  (cond
    [(string-prefix? "anthropic/" м) м]
    [(string-prefix? "claude" м) (string-append "anthropic/" м)]
    [else м]))

(define (спросить-модель указание модель теплота сообщения)
  (let* ((м (нормализовать-модель модель))
         (тело (jsexpr->bytes
                (hash 'model м
                      'temperature теплота
                      'messages (list->vector
                                 (cons (hash 'role "system" 'content указание)
                                       сообщения)))))
         (заголовки (list (string-append "Authorization: Bearer " (ключ-openrouter))
                          "Content-Type: application/json"))
         (порт (post-pure-port (string->url api-url) тело заголовки))
         (ответ (bytes->jsexpr (port->bytes порт))))
    (close-input-port порт)
    (let* ((варианты (hash-ref ответ 'choices #f))
           (первый  (and варианты (> (vector-length варианты) 0) (vector-ref варианты 0)))
           (сообщ   (and первый (hash-ref первый 'message #f)))
           (текст   (and сообщ (hash-ref сообщ 'content #f))))
      (or текст "Нет ответа от модели."))))

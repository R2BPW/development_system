#lang racket/base

;;; Подмастерье — Telegram-бот-исполнитель (Racket-обёртка над SBCL)
;;; Принимает задачу от пользователя, прогоняет через вычислительный поток,
;;; возвращает ответ с гарантией доставки

(require json
         net/url
         racket/file
         racket/format
         racket/list
         racket/match
         racket/path
         racket/port
         racket/string
         racket/system)

(provide запустить-подмастерье
         выполнить-в-sbcl
         сформировать-ответ-подмастерье
         послать-сообщение-п
         получить-обновления-п
         разобрать-обновления-п
         извлечь-сообщения-п
         текущий-поток
         загрузить-поток-п
         путь-исполнителя
         каталог-образов
         каталог-потоков-п
         восстановить-из-образа
         загрузить-активный-поток)

;; --- настройки ---

(define (ключ-бота)
  (or (getenv "APPRENTICE_BOT_TOKEN")
      (error 'ключ-бота "Не задана переменная APPRENTICE_BOT_TOKEN")))

(define (адрес-api метод)
  (string-append "https://api.telegram.org/bot" (ключ-бота) "/" метод))

(define путь-исполнителя
  (make-parameter
   (build-path (current-directory) "подмастерье" "исполнитель.lisp")))

(define каталог-образов
  (make-parameter
   (build-path (current-directory) "мастер" "образы")))

;; --- текущий поток ---

;; (список путь-к-файлу имя-пакета) или #f
(define текущий-поток (make-parameter #f))

(define каталог-потоков-п
  (make-parameter
   (build-path (current-directory) "мастер" "потоки")))

(define (загрузить-поток-п путь-потока имя-пакета)
  (текущий-поток (list путь-потока имя-пакета)))

(define (загрузить-активный-поток)
  (let ((путь (build-path (каталог-потоков-п) "активный.scm")))
    (if (not (file-exists? путь))
        #f
        (with-handlers ([exn:fail? (lambda (_) #f)])
          (let ((данные (call-with-input-file путь read)))
            (match данные
              [(list 'активный поля ...)
               (let ((файл #f) (пакет #f))
                 (for ([поле (in-list поля)])
                   (match поле
                     [(list 'файл з) (set! файл з)]
                     [(list 'пакет з) (set! пакет з)]
                     [_ (void)]))
                 (when (and файл пакет)
                   (загрузить-поток-п файл пакет))
                 (текущий-поток))]
              [_ #f]))))))

;; --- разбор обновлений (аналог Мастера) ---

(define (разобрать-обновления-п текст)
  (with-handlers ([exn:fail? (lambda (_) '())])
    (let ((данные (string->jsexpr текст)))
      (if (and (hash? данные)
               (hash-ref данные 'ok #f))
          (hash-ref данные 'result '())
          '()))))

(define (извлечь-сообщения-п обновления)
  (for/list ([обн (in-list обновления)]
             #:when (and (hash? обн)
                         (hash-has-key? обн 'message)))
    (let* ((сообщ (hash-ref обн 'message))
           (ид-обн (hash-ref обн 'update_id 0))
           (чат (hash-ref сообщ 'chat (hash)))
           (ид-чата (hash-ref чат 'id 0))
           (текст (hash-ref сообщ 'text "")))
      (list ид-обн ид-чата текст))))

;; --- выполнение задачи в SBCL ---

(define (выполнить-в-sbcl путь-потока имя-пакета задача)
  (let* ((путь-sbcl (find-executable-path "sbcl"))
         (исп (path->string (путь-исполнителя)))
         (загрузка (format "(load ~s :verbose nil :print nil)" исп))
         (вызов (format "(let* ((ф (funcall (intern \"ЗАПУСК\" \"ИСПОЛНИТЕЛЬ\") ~s ~s ~s))) (princ ф) (finish-output) (sb-ext:exit :code 0))"
                         (path->string путь-потока)
                         имя-пакета
                         задача)))
    (unless путь-sbcl
      (error 'выполнить-в-sbcl "Программа sbcl не найдена в PATH"))
    (let-values ([(процесс выход вход ошибки)
                  (subprocess #f #f #f
                              путь-sbcl
                              "--noinform" "--non-interactive"
                              "--eval" загрузка
                              "--eval" вызов)])
      (close-output-port вход)
      (let ((вывод (port->string выход))
            (ош (port->string ошибки)))
        (close-input-port выход)
        (close-input-port ошибки)
        (subprocess-wait процесс)
        (let ((код (subprocess-status процесс)))
          (if (zero? код)
              (string-trim вывод)
              (format "Ошибка SBCL (код ~a): ~a" код (string-trim ош))))))))

;; --- восстановление из образа ---

(define (восстановить-из-образа путь-образа задача)
  (let* ((путь-sbcl (find-executable-path "sbcl"))
         (выражение
          (format "(progn (let ((р (исполнитель:выполнить-из-образа ~s))) (princ р) (finish-output) (sb-ext:exit :code 0)))"
                  задача)))
    (unless путь-sbcl
      (error 'восстановить-из-образа "Программа sbcl не найдена в PATH"))
    (let-values ([(процесс выход вход ошибки)
                  (subprocess #f #f #f
                              путь-sbcl
                              "--noinform" "--non-interactive"
                              "--core" (path->string путь-образа)
                              "--eval" выражение)])
      (close-output-port вход)
      (let ((вывод (port->string выход))
            (ош (port->string ошибки)))
        (close-input-port выход)
        (close-input-port ошибки)
        (subprocess-wait процесс)
        (let ((код (subprocess-status процесс)))
          (if (zero? код)
              (string-trim вывод)
              (format "Ошибка восстановления (код ~a): ~a" код (string-trim ош))))))))

;; --- формирование ответа ---

(define (сформировать-ответ-подмастерье текст)
  (загрузить-активный-поток)
  (let ((поток (текущий-поток)))
    (cond
      [(string=? текст "/состояние")
       (if поток
           (format "Активный поток: ~a\nПакет: ~a"
                   (if (path? (first поток))
                       (path->string (first поток))
                       (first поток))
                   (second поток))
           "Поток не загружен.")]
      [(not поток)
       "Поток не загружен. Мастер должен назначить задачу."]
      [else
       (выполнить-в-sbcl (first поток) (second поток) текст)])))

;; --- отправка ---

(define (послать-сообщение-п ид-чата текст)
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

(define (получить-обновления-п сдвиг)
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

(define (запустить-подмастерье)
  (displayln "Подмастерье запущен. Ожидаю задачи...")
  (загрузить-активный-поток)
  (let цикл ((сдвиг 0))
    (with-handlers ([exn:fail:network?
                     (lambda (e)
                       (displayln (format "Ошибка сети: ~a" (exn-message e)))
                       (sleep 5)
                       (цикл сдвиг))])
      (let* ((ответ (получить-обновления-п сдвиг))
             (обновления (разобрать-обновления-п ответ))
             (сообщения (извлечь-сообщения-п обновления)))
        (let обр ((ост сообщения) (макс-сдвиг сдвиг))
          (if (null? ост)
              (цикл макс-сдвиг)
              (let* ((сообщ (car ост))
                     (ид-обн (first сообщ))
                     (ид-чата (second сообщ))
                     (текст (third сообщ))
                     (ответ-текст (сформировать-ответ-подмастерье текст)))
                (displayln (format "< ~a" текст))
                (displayln (format "> ~a" ответ-текст))
                (послать-сообщение-п ид-чата ответ-текст)
                (обр (cdr ост) (max макс-сдвиг (+ ид-обн 1))))))))))

;; --- точка входа ---

(module+ main
  (запустить-подмастерье))

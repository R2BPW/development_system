#lang racket/base

;;; Мастер — бот-оркестратор с inline-кнопками

(require json
         net/url
         racket/file
         racket/format
         racket/list
         racket/match
         racket/path
         racket/port
         racket/runtime-path
         racket/string
         "dusha.rkt"
         "potoki.rkt"
         "kombinirovanie.rkt"
         "llm.rkt"
         "report.rkt")

(provide разобрать-обновления
         извлечь-события
         сформировать-ответ
         послать-сообщение
         послать-с-кнопками
         получить-обновления
         запустить-опрос
         загрузить-душу
         текущая-душа
         каталог-потоков
         обработать-породить
         обработать-потоки
         обработать-запустить
         обработать-остановить
         обработать-переключить
         обработать-использовать
         обработать-состояние
         обработать-диалог)

;; --- настройки ---

(define (ключ-бота)
  (or (getenv "MASTER_BOT_TOKEN")
      (error 'ключ-бота "Не задана переменная MASTER_BOT_TOKEN")))

(define (ид-админа)
  (let ((s (getenv "ADMIN_CHAT_ID")))
    (and s (string->number s))))

(define (админ? ид-чата)
  (let ((а (ид-админа)))
    (or (not а) (= ид-чата а))))

(define (адрес-api метод)
  (string-append "https://api.telegram.org/bot" (ключ-бота) "/" метод))

(define-runtime-path корень ".")
(define (путь-души) (build-path корень "душа.scm"))
(define (каталог-истории) (build-path корень "история"))
(define каталог-потоков (make-parameter (build-path корень "потоки")))

;; --- душа ---

(define текущая-душа (make-parameter #f))

(define (загрузить-душу)
  (текущая-душа (прочитать-душу (путь-души))))

;; --- состояние: ожидание ввода описания ---

(define *ожидает-породить* (make-hash))   ; ид-чата → #t

;; --- разбор обновлений ---

(define (разобрать-обновления текст)
  (with-handlers ([exn:fail? (lambda (_) '())])
    (let ((данные (string->jsexpr текст)))
      (if (and (hash? данные) (hash-ref данные 'ok #f))
          (hash-ref данные 'result '())
          '()))))

;; Возвращает события двух типов:
;;   (list ид-обн ид-чата 'message текст)
;;   (list ид-обн ид-чата 'callback callback-id данные)
(define (извлечь-события обновления)
  (filter-map
   (lambda (обн)
     (cond
       [(hash-has-key? обн 'message)
        (let* ((сообщ   (hash-ref обн 'message))
               (ид-обн  (hash-ref обн 'update_id 0))
               (чат     (hash-ref сообщ 'chat (hash)))
               (ид-чата (hash-ref чат 'id 0))
               (текст   (hash-ref сообщ 'text "")))
          (list ид-обн ид-чата 'message текст))]
       [(hash-has-key? обн 'callback_query)
        (let* ((cb      (hash-ref обн 'callback_query))
               (ид-обн  (hash-ref обн 'update_id 0))
               (чат     (hash-ref (hash-ref cb 'message (hash)) 'chat (hash)))
               (ид-чата (hash-ref чат 'id 0))
               (cb-id   (hash-ref cb 'id ""))
               (данные  (hash-ref cb 'data "")))
          (list ид-обн ид-чата 'callback cb-id данные))]
       [else #f]))
   обновления))

;; обратная совместимость
(define (извлечь-сообщения обн)
  (filter-map
   (lambda (с) (and (eq? (third с) 'message) с))
   (извлечь-события обн)))

;; --- reply-клавиатура (снизу экрана) ---

(define (кнопка-reply текст)
  (hash 'text текст))

(define (главное-меню-reply)
  (list (list (кнопка-reply "🔧 Породить поток") (кнопка-reply "📋 Мои потоки"))
        (list (кнопка-reply "ℹ️ Состояние")      (кнопка-reply "⏹ Остановить"))
        (list (кнопка-reply "🧠 Душа")            (кнопка-reply "💾 Память"))
        (list (кнопка-reply "🕐 Время"))))

;; --- отправка ---

(define (послать-запрос метод тело)
  (let* ((адрес    (string->url (адрес-api метод)))
         (заголовки '("Content-Type: application/json"))
         (порт     (post-pure-port адрес (string->bytes/utf-8 тело) заголовки)))
    (begin0 (port->string порт) (close-input-port порт))))

(define (послать-сообщение ид-чата текст)
  (послать-запрос "sendMessage"
                  (jsexpr->string (hash 'chat_id ид-чата 'text текст))))

(define (послать-документ ид-чата путь подпись)
  (let* ((данные  (file->bytes путь))
         (имя-ф   (path->string (file-name-from-path путь)))
         (граница "----RktBoundary7483920")
         (тело    (bytes-append
                   (string->bytes/utf-8
                    (string-append
                     "--" граница "\r\n"
                     "Content-Disposition: form-data; name=\"chat_id\"\r\n\r\n"
                     (number->string ид-чата) "\r\n"
                     "--" граница "\r\n"
                     "Content-Disposition: form-data; name=\"caption\"\r\n\r\n"
                     подпись "\r\n"
                     "--" граница "\r\n"
                     "Content-Disposition: form-data; name=\"document\"; filename=\""
                     имя-ф "\"\r\n"
                     "Content-Type: application/pdf\r\n\r\n"))
                   данные
                   (string->bytes/utf-8
                    (string-append "\r\n--" граница "--\r\n"))))
         (адрес   (string->url (адрес-api "sendDocument")))
         (заголовки (list (string-append
                           "Content-Type: multipart/form-data; boundary="
                           граница)))
         (порт    (post-pure-port адрес тело заголовки)))
    (begin0 (port->string порт) (close-input-port порт))))

(define (послать-с-кнопками ид-чата текст)
  (послать-запрос "sendMessage"
                  (jsexpr->string
                   (hash 'chat_id ид-чата
                         'text текст
                         'reply_markup (hash 'keyboard (главное-меню-reply)
                                             'resize_keyboard #t
                                             'persistent #t)))))

(define (ответить-на-кнопку cb-id)
  (послать-запрос "answerCallbackQuery"
                  (jsexpr->string (hash 'callback_query_id cb-id))))

;; --- получение обновлений ---

(define (получить-обновления сдвиг)
  (let* ((адрес (string->url
                 (string-append (адрес-api "getUpdates")
                                "?timeout=30&offset="
                                (number->string сдвиг))))
         (порт (get-pure-port адрес)))
    (begin0 (port->string порт) (close-input-port порт))))

;; --- обработка команд ---

(define (сформировать-ответ текст)
  (cond
    [(or (string=? текст "/start") (string=? текст "/меню")) #f]
    [(or (string=? текст "🕐 Время")       (string=? текст "/время"))     (формат-время)]
    [(or (string=? текст "ℹ️ Состояние")   (string=? текст "/состояние")) (обработать-состояние)]
    [(or (string=? текст "🧠 Душа")         (string=? текст "/душа"))      (обработать-душу)]
    [(or (string=? текст "💾 Память")       (string=? текст "/память"))    (обработать-память)]
    [(or (string=? текст "📋 Мои потоки")  (string=? текст "/потоки"))    (обработать-потоки)]
    [(or (string=? текст "⏹ Остановить")   (string=? текст "/остановить")) (обработать-остановить)]
    [(string=? текст "/отчёт") (обработать-отчёт)]
    [(or (string=? текст "🔧 Породить поток")) #f]   ; ждать ввода
    [(string-prefix? текст "/породить ")
     (обработать-породить (substring текст 10))]
    [(string-prefix? текст "/запустить ")
     (обработать-запустить (string-trim (substring текст 11)))]
    [(string-prefix? текст "/переключить ")
     (обработать-переключить (string-trim (substring текст 13)))]
    [(string-prefix? текст "/использовать ")
     (обработать-использовать (string-trim (substring текст 14)))]
    [else (обработать-диалог текст)]))

(define (обработать-callback данные)
  (cond
    [(string=? данные "menu")           #f]       ; показать главное меню
    [(string=? данные "cmd:породить")   'породить] ; ждать ввода
    [(string=? данные "cmd:потоки")     (обработать-потоки)]
    [(string=? данные "cmd:состояние")  (обработать-состояние)]
    [(string=? данные "cmd:остановить") (обработать-остановить)]
    [(string=? данные "cmd:душа")       (обработать-душу)]
    [(string=? данные "cmd:память")     (обработать-память)]
    [(string=? данные "cmd:время")      (формат-время)]
    [(string-prefix? данные "запустить:")
     (обработать-запустить (substring данные 11))]
    [else #f]))

(define (обработать-диалог текст)
  (let ((д (текущая-душа)))
    (if (not д)
        "Душа не загружена."
        (with-handlers ([exn:fail?
                         (lambda (e)
                           (format "Ошибка модели: ~a" (exn-message e)))])
          (let* ((указание (or (получить-поле д 'указание) "Ты помощник."))
                 (модель   (or (получить-поле д 'модель) "anthropic/claude-3-5-haiku"))
                 (теплота  (or (получить-поле д 'теплота) 0.7))
                 (девиз    (or (получить-поле д 'девиз) ""))
                 (полное   (if (string=? девиз "")
                               указание
                               (string-append указание "\nДевиз: " девиз))))
            (спросить-модель полное модель теплота
                             (list (hash 'role "user" 'content текст))))))))

(define (обработать-породить описание)
  (let ((д (текущая-душа)))
    (if (not д)
        "Душа не загружена."
        (with-handlers ([exn:fail?
                         (lambda (e) (format "Ошибка порождения: ~a" (exn-message e)))])
          (let* ((р      (породить-поток д описание (каталог-потоков)))
                 (успех? (first р))
                 (имя    (second р))
                 (путь   (third р)))
            (if успех?
                (format "Поток ~a порождён и проверен.\nФайл: ~a" имя путь)
                (format "Поток ~a порождён, но проверка не пройдена.\nФайл: ~a" имя путь)))))))

(define (обработать-душу)
  (let ((д (текущая-душа)))
    (if д (показать-душу д) "Душа не загружена.")))

(define (обработать-память)
  (let ((д (текущая-душа)))
    (if д (показать-память д) "Душа не загружена.")))

(define (обработать-состояние)
  (let ((актив (прочитать-активный (каталог-потоков))))
    (if актив
        (format "Активный поток: ~a\nПакет: ~a" (first актив) (third актив))
        "Нет активного потока.")))

(define (обработать-потоки)
  (let ((потоки (список-потоков (каталог-потоков))))
    (if (null? потоки)
        "Потоков нет."
        (let ((актив (прочитать-активный (каталог-потоков))))
          (string-join
           (for/list ([п (in-list потоки)])
             (let* ((имя      (first п))
                    (описание (second п))
                    (значок   (if (and актив (string=? имя (first актив))) "▶ " "  ")))
               (format "~a~a — ~a" значок имя описание)))
           "\n")))))

(define (обработать-запустить имя)
  (let ((поток (найти-поток (каталог-потоков) имя)))
    (if (not поток)
        (format "Поток ~a не найден." имя)
        (let* ((актив  (прочитать-активный (каталог-потоков)))
               (мета   (third поток))
               (пакет  (string-append "поток-" (or (and мета (поле-меты мета 'имя)) имя))))
          (if (and актив (string=? (first актив) имя))
              (format "Поток ~a уже запущен." имя)
              (begin (записать-активный (каталог-потоков) имя пакет)
                     (format "Поток ~a запущен." имя)))))))

(define (обработать-остановить)
  (let ((актив (прочитать-активный (каталог-потоков))))
    (if (not актив)
        "Нет активного потока."
        (begin (удалить-активный (каталог-потоков))
               (format "Поток ~a остановлен." (first актив))))))

(define (обработать-переключить имя)
  (let ((актив (прочитать-активный (каталог-потоков))))
    (when актив (удалить-активный (каталог-потоков))))
  (обработать-запустить имя))

(define (обработать-использовать имя)
  (let ((поток (найти-поток (каталог-потоков) имя)))
    (if (not поток)
        (format "Поток ~a не найден." имя)
        (let* ((мета (third поток))
               (описание (if мета (or (поле-меты мета 'описание) "—") "—")))
          (string-append (обработать-запустить имя)
                         (format "\nОписание: ~a" описание))))))

(define (обработать-отчёт)
  "Генерирует PDF из трасс и сообщает путь."
  (with-handlers ([exn:fail?
                   (lambda (e) (format "Ошибка отчёта: ~a" (exn-message e)))])
    (let ((пдф (собрать-отчёт)))
      (if пдф
          (format "Отчёт готов: ~a" (path->string пдф))
          "Нет данных трассировки. Запустите поток и выполните задачу."))))

(define (формат-время)
  (let ((с (seconds->date (current-seconds))))
    (format "~a-~a-~a ~a:~a:~a"
            (date-year с) (~два (date-month с)) (~два (date-day с))
            (~два (date-hour с)) (~два (date-minute с)) (~два (date-second с)))))

(define (~два ч)
  (~a ч #:min-width 2 #:pad-string "0" #:align 'right))

;; --- цикл опроса ---

(define (запустить-опрос)
  (загрузить-душу)
  (displayln "Мастер запущен. Ожидаю сообщения...")
  (let цикл ((сдвиг 0))
    (with-handlers ([exn:fail?
                     (lambda (e)
                       (displayln (format "Ошибка: ~a" (exn-message e)))
                       (sleep 5)
                       (цикл сдвиг))])
      (let* ((ответ    (получить-обновления сдвиг))
             (события  (извлечь-события (разобрать-обновления ответ))))
        (let обр ((ост события) (макс сдвиг))
          (if (null? ост)
              (цикл макс)
              (let* ((событие (car ост))
                     (ид-обн  (first событие))
                     (ид-чата (second событие))
                     (тип     (third событие)))
                (when (and (админ? ид-чата) (eq? тип 'message))
                  (let ((текст (fourth событие)))
                    (displayln (format "< ~a" текст))
                    (cond
                      [(or (string=? текст "/start") (string=? текст "/меню"))
                       (послать-с-кнопками ид-чата "Выберите действие:")]
                      [(string=? текст "🔧 Породить поток")
                       (hash-set! *ожидает-породить* ид-чата #t)
                       (послать-сообщение ид-чата "Опишите задачу для нового потока:")]
                      [(hash-ref *ожидает-породить* ид-чата #f)
                       (hash-remove! *ожидает-породить* ид-чата)
                       (with-handlers ([exn:fail?
                                        (lambda (e)
                                          (послать-сообщение ид-чата
                                            (format "Ошибка порождения: ~a" (exn-message e))))])
                         (let* ((д      (текущая-душа))
                                (р      (породить-поток д текст (каталог-потоков)))
                                (успех? (first р))
                                (имя    (second р))
                                (путь   (third р))
                                (статус (if успех? "✅ проверен" "⚠️ проверка не пройдена"))
                                (сообщ  (format "Поток «~a» порождён (~a)." имя статус)))
                           (displayln (format "> ~a" сообщ))
                           (послать-с-кнопками ид-чата сообщ)
                           ;; Гарантированно шлём код файлом
                           (when (file-exists? путь)
                             (послать-документ ид-чата путь
                               (format "~a.lisp — ~a" имя текст)))))]
                      [(string=? текст "/отчёт")
                       (with-handlers ([exn:fail?
                                        (lambda (e)
                                          (послать-сообщение ид-чата
                                            (format "Ошибка: ~a" (exn-message e))))])
                         (let ((пдф (собрать-отчёт)))
                           (if пдф
                               (послать-документ ид-чата пдф "Журнал вычислений")
                               (послать-сообщение ид-чата
                                 "Нет данных трассировки."))))]
                      [else
                       (let ((р (сформировать-ответ текст)))
                         (displayln (format "> ~a" (or р "")))
                         (записать-историю (каталог-истории) ид-чата текст (or р ""))
                         (when р (послать-сообщение ид-чата р)))])))
                (обр (cdr ост) (max макс (+ ид-обн 1))))))))))

;; --- точка входа ---

(module+ main
  (запустить-опрос))

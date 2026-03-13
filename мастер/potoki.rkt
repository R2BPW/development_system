#lang racket/base

;;; Потоки — порождение и проверка вычислительных потоков

(require racket/file
         racket/format
         racket/match
         racket/path
         racket/port
         racket/string
         racket/system
         "dusha.rkt")

(provide спросить-клод
         сформировать-задание
         записать-поток
         записать-мета
         проверить-поток
         породить-поток
         извлечь-код
         имя-пакета
         список-потоков
         прочитать-мету
         найти-поток
         записать-активный
         прочитать-активный
         удалить-активный
         поле-меты)

;; --- вызов Claude Code ---

(define (спросить-клод вопрос)
  (let ((путь-клод (find-executable-path "claude")))
    (unless путь-клод
      (error 'спросить-клод "Программа claude не найдена в PATH"))
    (let-values ([(процесс выход вход ошибки)
                  (subprocess #f #f #f
                              путь-клод
                              "--print" вопрос)])
      (close-output-port вход)
      (let ((ответ (port->string выход)))
        (close-input-port выход)
        (close-input-port ошибки)
        (subprocess-wait процесс)
        (let ((код (subprocess-status процесс)))
          (if (zero? код)
              ответ
              (error 'спросить-клод "Claude Code завершился с кодом ~a" код)))))))

;; --- формирование задания ---

(define (сформировать-задание душа описание)
  (let ((указание (or (получить-поле душа 'указание) ""))
        (девиз (or (получить-поле душа 'девиз) ""))
        (модель (or (получить-поле душа 'модель) "")))
    (string-append
     "Ты генератор вычислительных потоков на Common Lisp.\n"
     "Системное указание: " указание "\n"
     "Девиз: " девиз "\n\n"
     "Задача: " описание "\n\n"
     "Требования к коду:\n"
     "- В начале файла после defpackage/in-package добавь: (ql:quickload '(\"drakma\" \"cl-json\" \"flexi-streams\") :silent t)\n"
     "- Для HTTP используй ТОЛЬКО drakma:http-request, никакого dexador/dex\n"
     "- Для JSON используй ТОЛЬКО cl-json: cl-json:encode-json-to-string, cl-json:decode-json-from-string\n"
     "- Для getenv используй ТОЛЬКО uiop:getenv, переменная окружения называется OPENROUTER_API_KEY\n"
     "- Создай пакет с именем :поток-<имя> где <имя> — краткое слово\n"
     "- Обязательная функция (defun выполнить (задача) ...) — точка входа\n"
     "- Рекурсия вместо loop, функции до 15 строк\n"
     "- Русские имена для прикладных понятий\n"
     "- Формат файла: заголовок с -*- Mode: Lisp -*-, имя, описание\n"
     "- restart-case для обработки ошибок\n"
     "- ТРАССИРОВКА: если пакет :трас доступен (find-package :трас), используй его:\n"
     "  В начале выполнить: (when (find-package :трас) (funcall (intern \"НАЧАТЬ-СЛЕД\" :трас) \"имя-потока\" задача))\n"
     "  После каждого вызова модели (шаг N): (when (find-package :трас) (funcall (intern \"ЗАПИСАТЬ-ШАГ\" :трас) N вход ответ :качество :достаточно-или-нет))\n"
     "  В конце выполнить: (when (find-package :трас) (funcall (intern \"ЗАВЕРШИТЬ-СЛЕД\" :трас) итог))\n"
     "- Верни ТОЛЬКО код Common Lisp без единого слова пояснений\n"
     "- НЕ пиши планы, НЕ проси подтверждения, НЕ объясняй структуру\n"
     "- НЕ используй markdown-блоки ```lisp — только голый код\n"
     "- Первая строка файла: ;;; -*- Mode: Lisp -*- Последняя: закрывающая скобка\n")))

;; --- извлечение кода из ответа ---

(define (извлечь-код ответ)
  (let ((строки (string-split ответ "\n")))
    (let ищи ((ост строки) (внутри #f) (собрано '()))
      (cond
        [(null? ост)
         (if (null? собрано)
             (string-trim ответ)
             (string-join (reverse собрано) "\n"))]
        [(and (not внутри)
              (let ((с (string-trim (car ост))))
                (or (string-prefix? с "```lisp")
                    (string-prefix? с "```common-lisp")
                    (string-prefix? с "```cl"))))
         (ищи (cdr ост) #t собрано)]
        [(and внутри (string-prefix? (string-trim (car ост)) "```"))
         (ищи (cdr ост) #f собрано)]
        [внутри
         (ищи (cdr ост) #t (cons (car ост) собрано))]
        [else
         (ищи (cdr ост) #f собрано)]))))

;; --- имя пакета из кода ---

(define (имя-пакета код)
  (let ((совп (regexp-match #px":поток-([a-zA-Zа-яёА-ЯЁ0-9_-]+)" код)))
    (if совп
        (string-trim (cadr совп))
        "безымянный")))

;; --- запись файлов ---

(define (записать-поток каталог имя код)
  (let ((путь (build-path каталог (string-append имя ".lisp"))))
    (unless (directory-exists? каталог)
      (make-directory* каталог))
    (call-with-output-file путь
      (lambda (порт) (display код порт) (newline порт))
      #:exists 'replace)
    путь))

(define (записать-мета каталог имя описание)
  (let ((путь (build-path каталог (string-append имя ".мета")))
        (с (seconds->date (current-seconds))))
    (call-with-output-file путь
      (lambda (порт)
        (fprintf порт "(мета\n")
        (fprintf порт "  (имя         ~s)\n" имя)
        (fprintf порт "  (создан      ~s)\n"
                 (format "~a-~a-~aT~a:~a:~a"
                         (date-year с)
                         (~a (date-month с) #:min-width 2 #:pad-string "0" #:align 'right)
                         (~a (date-day с) #:min-width 2 #:pad-string "0" #:align 'right)
                         (~a (date-hour с) #:min-width 2 #:pad-string "0" #:align 'right)
                         (~a (date-minute с) #:min-width 2 #:pad-string "0" #:align 'right)
                         (~a (date-second с) #:min-width 2 #:pad-string "0" #:align 'right)))
        (fprintf порт "  (описание    ~s)\n" описание)
        (fprintf порт "  (зависимости ())\n")
        (fprintf порт "  (входит-в    ())\n")
        (fprintf порт "  (состояние   :готов))\n"))
      #:exists 'replace)
    путь))

;; --- проверка потока в SBCL ---

(define (проверить-поток путь)
  (let* ((путь-sbcl (find-executable-path "sbcl"))
         (ql-init (path->string
                   (build-path (find-system-path 'home-dir) "quicklisp" "setup.lisp")))
         (загрузить-ql (format "(when (probe-file ~s) (load ~s :verbose nil :print nil))" ql-init ql-init))
         (загрузить-зависимости "(when (find-package :ql) (funcall (intern \"QUICKLOAD\" :ql) '(\"drakma\" \"cl-json\" \"flexi-streams\" \"uiop\") :silent t))")
         (выражение
          (format "(multiple-value-bind (fasl warn fail) (compile-file ~s) (if fail (sb-ext:exit :code 1) (sb-ext:exit :code 0)))"
                  (path->string путь))))
    (unless путь-sbcl
      (error 'проверить-поток "Программа sbcl не найдена в PATH"))
    (let-values ([(процесс выход вход ошибки)
                  (subprocess #f #f #f
                              путь-sbcl
                              "--noinform" "--non-interactive"
                              "--eval" загрузить-ql
                              "--eval" загрузить-зависимости
                              "--eval" выражение)])
      (close-output-port вход)
      (let ((вывод (port->string выход))
            (ош (port->string ошибки)))
        (close-input-port выход)
        (close-input-port ошибки)
        (subprocess-wait процесс)
        (let ((код (subprocess-status процесс)))
          (values (zero? код) (string-append вывод ош)))))))

;; --- чтение метаданных потока ---

(define (прочитать-мету путь)
  (if (file-exists? путь)
      (with-handlers ([exn:fail? (lambda (_) #f)])
        (call-with-input-file путь read))
      #f))

(define (поле-меты мета имя)
  (match мета
    [(list 'мета поля ...)
     (for/or ([поле (in-list поля)])
       (match поле
         [(list (== имя) значение) значение]
         [_ #f]))]
    [_ #f]))

;; --- список потоков в каталоге ---

(define (список-потоков каталог)
  (if (not (directory-exists? каталог))
      '()
      (let ((файлы (directory-list каталог)))
        (for/list ([ф (in-list файлы)]
                   #:when (let ((с (path->string ф)))
                            (string-suffix? с ".мета")))
          (let* ((путь (build-path каталог ф))
                 (мета (прочитать-мету путь))
                 (имя (or (and мета (поле-меты мета 'имя)) "?"))
                 (описание (or (and мета (поле-меты мета 'описание)) ""))
                 (состояние (or (and мета (поле-меты мета 'состояние)) ':готов)))
            (list имя описание состояние))))))

;; --- поиск потока по имени ---

(define (найти-поток каталог имя)
  (let ((путь-lisp (build-path каталог (string-append имя ".lisp")))
        (путь-мета (build-path каталог (string-append имя ".мета"))))
    (if (file-exists? путь-lisp)
        (list путь-lisp путь-мета
              (if (file-exists? путь-мета)
                  (прочитать-мету путь-мета)
                  #f))
        #f)))

;; --- активный поток (общий файл для Подмастерья) ---

(define (записать-активный каталог имя пакет)
  (let ((путь (build-path каталог "активный.scm")))
    (call-with-output-file путь
      (lambda (порт)
        (fprintf порт "(активный\n")
        (fprintf порт "  (имя    ~s)\n" имя)
        (fprintf порт "  (файл   ~s)\n"
                 (path->string (build-path каталог (string-append имя ".lisp"))))
        (fprintf порт "  (пакет  ~s))\n" пакет))
      #:exists 'replace)
    путь))

(define (прочитать-активный каталог)
  (let ((путь (build-path каталог "активный.scm")))
    (if (file-exists? путь)
        (with-handlers ([exn:fail? (lambda (_) #f)])
          (let ((данные (call-with-input-file путь read)))
            (match данные
              [(list 'активный поля ...)
               (let ((имя #f) (файл #f) (пакет #f))
                 (for ([поле (in-list поля)])
                   (match поле
                     [(list 'имя з) (set! имя з)]
                     [(list 'файл з) (set! файл з)]
                     [(list 'пакет з) (set! пакет з)]
                     [_ (void)]))
                 (if (and имя файл пакет)
                     (list имя файл пакет)
                     #f))]
              [_ #f])))
        #f)))

(define (удалить-активный каталог)
  (let ((путь (build-path каталог "активный.scm")))
    (when (file-exists? путь)
      (delete-file путь))))

;; --- главная функция порождения ---

(define (породить-поток душа описание каталог)
  (let* ((задание (сформировать-задание душа описание))
         (ответ (спросить-клод задание))
         (код (извлечь-код ответ))
         (имя (имя-пакета код))
         (путь-потока (записать-поток каталог имя код))
         (путь-мета (записать-мета каталог имя описание)))
    (let-values ([(успех? вывод) (проверить-поток путь-потока)])
      (list успех? имя путь-потока путь-мета вывод))))

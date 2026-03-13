#lang racket/base

;;; generation.rkt — тесты генерации потока
;;;
;;; LLM замокан: вместо вызова Claude используем пре-бейкнутый код.
;;; Проверяем:
;;;   B — компилируется + есть выполнить + нет CLOS
;;;   C — запускается с тестовым входом + возвращает осмысленный вывод

(require rackunit
         rackunit/text-ui
         racket/file
         racket/string
         racket/system
         racket/path
         racket/port
         racket/list
         (file "../мастер/potoki.rkt")
         (file "../мастер/dusha.rkt"))

;; ── мок-ответ LLM ────────────────────────────────────────────
;; Сгенерированный поток «питон» — выполняет Python-задачи через LLM
;; Это то что Мастер должен уметь породить по запросу вида:
;; "принять задачу строкой, спросить LLM, запустить Python, вернуть вывод"

(define MOCK-FIXTURE-PATH
  "/root/.openclaw/workspace/development_system/мастер/потоки/питон.lisp")

(define MOCK-CODE (file->string MOCK-FIXTURE-PATH))

;; Полный mock-ответ как от LLM (код в ```-блоке)
(define MOCK-LLM-RESPONSE
  (string-append "```lisp\n" MOCK-CODE "\n```"))

;; ── вспомогательные ──────────────────────────────────────────

(define (создать-вр-каталог)
  (make-temporary-file "тест-генерация~a" 'directory))

(define (запустить-поток путь задача)
  (define ql  (build-path (find-system-path 'home-dir) "quicklisp" "setup.lisp"))
  (define апи (or (getenv "OPENROUTER_API_KEY") ""))
  (define код
    (string-append
     "(when (probe-file \"" (path->string ql) "\") (load \"" (path->string ql) "\" :verbose nil))"
     "(require :uiop)"
     "(when (find-package :ql) (funcall (intern \"QUICKLOAD\" :ql)"
     "  '(\"drakma\" \"cl-json\" \"flexi-streams\") :silent t))"
     "(load \"" (path->string путь) "\" :verbose nil :print nil)"
     "(let ((r (funcall (intern \"ВЫПОЛНИТЬ\""
     "  (find-package-if-exists-or-error (find-package :поток-питон) :поток-питон))"
     "  \"" задача "\")))"
     "  (princ r) (finish-output) (sb-ext:exit :code 0))"))
  (with-output-to-string
    (lambda ()
      (parameterize ([current-environment-variables
                      (environment-variables-copy (current-environment-variables))])
        (putenv "OPENROUTER_API_KEY" апи)
        (system* (find-executable-path "sbcl")
                 "--noinform" "--non-interactive"
                 "--eval" (string-append
                           "(when (probe-file \"" (path->string ql) "\")"
                           "  (load \"" (path->string ql) "\" :verbose nil))")
                 "--eval" "(require :uiop)"
                 "--eval" (string-append
                           "(when (find-package :ql)"
                           "  (funcall (intern \"QUICKLOAD\" :ql)"
                           "    '(\"drakma\" \"cl-json\" \"flexi-streams\") :silent t))")
                 "--eval" (string-append "(load \"" (path->string путь) "\" :verbose nil :print nil)")
                 "--eval" (string-append
                           "(let ((r (funcall (intern \"ВЫПОЛНИТЬ\" :поток-питон) \""
                           задача "\")))"
                           "  (princ r) (finish-output) (sb-ext:exit :code 0)))"))))))

;; ── Suite B: структура сгенерированного файла ─────────────────

(define suite-B-структура
  (test-suite "B: структура потока"

    (test-case "извлечь-код извлекает код из мок-ответа"
      (define код (string-trim (извлечь-код MOCK-LLM-RESPONSE)))
      (check-true (string-contains? код "defpackage")
                  "должен содержать defpackage"))

    (test-case "имя-пакета определяет имя из извлечённого кода"
      (define код (извлечь-код MOCK-LLM-RESPONSE))
      (define имя (имя-пакета код))
      (check-false (string=? имя "безымянный")
                   "имя пакета должно быть определено"))

    (test-case "записать-поток сохраняет файл"
      (define вр (создать-вр-каталог))
      (define код (извлечь-код MOCK-LLM-RESPONSE))
      (define имя (имя-пакета код))
      (define путь (записать-поток вр имя код))
      (check-true (file-exists? путь) "файл должен существовать")
      (check-true (> (file-size путь) 0) "файл не должен быть пустым")
      (delete-directory/files вр))

    (test-case "код содержит функцию выполнить"
      (define код (извлечь-код MOCK-LLM-RESPONSE))
      (check-true (regexp-match? #rx"defun выполнить" код)
                  "должна быть функция (defun выполнить ...)"))

    (test-case "код не содержит CLOS (defclass/defmethod)"
      (define код (извлечь-код MOCK-LLM-RESPONSE))
      (check-false (regexp-match? #rx"defclass|defmethod" код)
                   "CLOS запрещён в потоках"))

    (test-case "проверить-поток: компиляция проходит без ошибок"
      (define вр (создать-вр-каталог))
      (define код (извлечь-код MOCK-LLM-RESPONSE))
      (define имя (имя-пакета код))
      (define путь (записать-поток вр имя код))
      (define-values (успех? вывод) (проверить-поток путь))
      (check-true успех?
                  (format "компиляция должна пройти; вывод: ~a" вывод))
      (delete-directory/files вр))))

;; ── Suite C: выполнение ───────────────────────────────────────

(define suite-C-выполнение
  (test-suite "C: выполнение с тестовым входом"

    (test-case "эхо-поток: выполнить возвращает строку"
      ;; Эхо не требует LLM — детерминированный тест
      (define эхо-путь
        "/root/.openclaw/workspace/development_system/мастер/потоки/эхо.lisp")
      (when (not (file-exists? эхо-путь))
        (error "эхо.lisp не найден, пропускаем"))
      (define ql (build-path (find-system-path 'home-dir) "quicklisp" "setup.lisp"))
      (define вывод
        (with-output-to-string
          (lambda ()
            (system* (find-executable-path "sbcl")
                     "--noinform" "--non-interactive"
                     "--eval" (string-append
                               "(when (probe-file \"" (path->string ql) "\")"
                               "  (load \"" (path->string ql) "\" :verbose nil))")
                     "--eval" "(require :uiop)"
                     "--eval" (string-append
                               "(load \"" эхо-путь "\" :verbose nil :print nil)")
                     "--eval" (string-append
                               "(let ((r (funcall (intern \"ВЫПОЛНИТЬ\" :поток-эхо) \"тест\")))"
                               "  (princ r) (finish-output) (sb-ext:exit :code 0))")))))
      (check-true (string-contains? вывод "тест")
                  (format "эхо должен вернуть вход; получено: ~s" вывод)))

    (test-case "питон-поток: компилируется и экспортирует выполнить"
      ;; Проверяем что потоком можно пользоваться (без вызова LLM внутри)
      (define ql (build-path (find-system-path 'home-dir) "quicklisp" "setup.lisp"))
      (define проверка
        (with-output-to-string
          (lambda ()
            (system* (find-executable-path "sbcl")
                     "--noinform" "--non-interactive"
                     "--eval" (string-append
                               "(when (probe-file \"" (path->string ql) "\")"
                               "  (load \"" (path->string ql) "\" :verbose nil))")
                     "--eval" "(require :uiop)"
                     "--eval" (string-append
                               "(when (find-package :ql)"
                               "  (funcall (intern \"QUICKLOAD\" :ql)"
                               "    '(\"drakma\" \"cl-json\" \"flexi-streams\") :silent t))")
                     "--eval" (string-append
                               "(load \"" MOCK-FIXTURE-PATH "\" :verbose nil :print nil)")
                     "--eval" (string-append
                               "(let ((sym (find-symbol \"ВЫПОЛНИТЬ\" :поток-питон)))"
                               "  (if (and sym (fboundp sym))"
                               "    (progn (princ \"OK\") (finish-output))"
                               "    (progn (princ \"MISSING\") (finish-output)))"
                               "  (sb-ext:exit :code 0))")))))
      (check-true (string-contains? проверка "OK")
                  "поток-питон должен экспортировать выполнить"))))

;; ── Запуск ────────────────────────────────────────────────────

(define all-suites (list suite-B-структура suite-C-выполнение))

(define (запустить!)
  (for/fold ([итог 0]) ([s (in-list all-suites)])
    (+ итог (run-tests s))))

(module+ main
  (exit (if (zero? (запустить!)) 0 1)))

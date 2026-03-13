#lang racket/base

;;; отчёт.rkt — генерация PDF-отчёта из трасс-файлов (.след)

(require racket/file
         racket/format
         racket/list
         racket/match
         racket/path
         racket/port
         racket/runtime-path
         racket/string
         racket/system)

(provide собрать-отчёт
         список-следов
         последний-след)

(define-runtime-path корень ".")

(define (каталог-следов)
  (build-path корень "следы"))

;; --- чтение .след файлов ---

(define (список-следов)
  "Все .след файлы, отсортированные по дате (новые первыми)."
  (let* ((к  (каталог-следов))
         (фс (if (directory-exists? к)
                 (directory-list к #:build? #t)
                 '())))
    (sort (filter (lambda (ф)
                    (string-suffix? (path->string ф) ".след"))
                  фс)
          (lambda (a b)
            (> (file-or-directory-modify-seconds a)
               (file-or-directory-modify-seconds b))))))

(define (последний-след)
  (let ((сл (список-следов)))
    (and (pair? сл) (car сл))))

(define (прочитать-след путь)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (call-with-input-file путь read)))

;; --- доступ к полям S-выражения ---

(define (поле-следа след имя)
  (match след
    [(list 'след поля ...)
     (for/or ([п (in-list поля)])
       (match п
         [(list (== имя) знач) знач]
         [(cons (== имя) знч) знч]
         [_ #f]))]
    [_ #f]))

(define (шаги-следа след)
  (let ((ш (поле-следа след 'шаги)))
    (if (list? ш) ш '())))

(define (поле-шага шаг имя)
  (match шаг
    [(list 'шаг поля ...)
     (for/or ([п (in-list поля)])
       (match п
         [(list (== имя) знач) знач]
         [_ #f]))]
    [_ #f]))

;; --- форматирование для LaTeX ---

(define (latex-escape s)
  (let ((str (if (string? s) s (~a s))))
    (foldl (lambda (пара рез)
              (string-replace рез (car пара) (cdr пара)))
            str
            '(("\\" . "\\textbackslash{}")
              ("&"  . "\\&")
              ("%"  . "\\%")
              ("$"  . "\\$")
              ("#"  . "\\#")
              ("_"  . "\\_")
              ("{"  . "\\{")
              ("}"  . "\\}")
              ("~"  . "\\textasciitilde{}")
              ("^"  . "\\textasciicircum{}")
              ("<"  . "\\textless{}")
              (">"  . "\\textgreater{}")))))

(define (шаг->latex шаг)
  (let* ((ном  (поле-шага шаг 'номер))
         (вход (latex-escape (or (поле-шага шаг 'вход) "")))
         (отв  (latex-escape (or (поле-шага шаг 'ответ) "")))
         (мс   (or (поле-шага шаг 'мс) 0))
         (кач  (~a (or (поле-шага шаг 'качество) ""))))
    (format "
\\subsection*{Шаг ~a (\\SI{~a}{мс}, качество: \\texttt{~a})}
\\textbf{Вход:}
\\begin{quote}
~a
\\end{quote}
\\textbf{Ответ модели:}
\\begin{lstlisting}[basicstyle=\\ttfamily\\small,breaklines=true]
~a
\\end{lstlisting}
"
            ном мс кач вход отв)))

(define (след->latex след)
  (let* ((поток (or (поле-следа след 'поток) "неизвестно"))
         (задача (latex-escape (or (поле-следа след 'задача) "")))
         (время (or (поле-следа след 'время) ""))
         (итог (latex-escape (or (поле-следа след 'итог) "")))
         (мс (or (поле-следа след 'мс-всего) 0))
         (итераций (or (поле-следа след 'итераций) 0))
         (шаги (шаги-следа след)))
    (string-append
     (format "
\\section{Вычисление потока «~a»}
\\textbf{Время:} ~a \\\\ \\textbf{Итераций:} ~a \\\\ \\textbf{Всего:} \\SI{~a}{мс}
\\subsection*{Задача}
\\begin{quote}
~a
\\end{quote}
"
             поток время итераций мс задача)
     (apply string-append (map шаг->latex шаги))
     (format "
\\subsection*{Итоговый ответ}
\\begin{mdframed}
~a
\\end{mdframed}
"
             итог))))

;; --- генерация LaTeX документа ---

(define (следы->latex пути)
  (let ((следы (filter-map прочитать-след пути)))
    (string-append
     "\\documentclass[11pt,a4paper]{article}
\\usepackage[T2A]{fontenc}
\\usepackage[utf8]{inputenc}
\\usepackage[russian]{babel}
\\usepackage{geometry}
\\usepackage{listings}
\\usepackage{mdframed}
\\usepackage{siunitx}
\\usepackage{xcolor}
\\usepackage{hyperref}
\\geometry{a4paper,left=2.5cm,right=2.5cm,top=2.5cm,bottom=2.5cm}
\\lstset{backgroundcolor=\\color{gray!8},frame=single,basicstyle=\\ttfamily\\small,breaklines=true}
\\title{Журнал вычислений development\\_system}
\\date{\\today}
\\begin{document}
\\maketitle
\\tableofcontents
\\newpage
"
     (apply string-append (map след->latex следы))
     "\\end{document}\n")))

;; --- компиляция PDF ---

(define (скомпилировать-pdf tex-путь)
  (let* ((каталог (path->string (path-only tex-путь)))
         (команда (format "cd ~s && lualatex -interaction=nonstopmode ~s"
                          каталог (path->string tex-путь))))
    (system команда)
    (system команда)  ; второй проход для оглавления
    (path-replace-extension tex-путь #".pdf")))

;; --- главная функция ---

(define (собрать-отчёт [пути #f])
  "Генерирует PDF из .след файлов. Без аргументов — все следы."
  (let* ((пт       (or пути (список-следов)))
         (tex-путь (build-path (каталог-следов) "отчёт.tex")))
    (unless (directory-exists? (каталог-следов))
      (make-directory* (каталог-следов)))
    (if (null? пт)
        #f
        (begin
          (call-with-output-file tex-путь
            (lambda (порт)
              (display (следы->latex пт) порт))
            #:exists 'replace)
          (скомпилировать-pdf tex-путь)))))

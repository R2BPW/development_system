;;; -*- Mode: Lisp; Coding: utf-8 -*-
;;; Имя: поток-агент-аналитик
;;; Описание: Агентная версия граф-аналитика. LLM строит план исследования через tool calls.
;;; Вход: mermaid-граф + путь-к-репо → tool-loop → структурированный отчёт

(defpackage :поток-агент-аналитик
  (:use :cl)
  (:export #:выполнить #:тест-всё))

(in-package :поток-агент-аналитик)

(ql:quickload '("dexador" "cl-json" "cl-ppcre") :silent t)

;; ════════════════════════════════════════════════════════════════
;; Конфигурация
;; ════════════════════════════════════════════════════════════════

(defparameter *макс-tool-calls* 30)

(defun ключ-апи ()
  (or (sb-ext:posix-getenv "OPENROUTER_API_KEY")
      (error "OPENROUTER_API_KEY не задан")))

(defun модель-апи ()
  (or (sb-ext:posix-getenv "ANALYZER_MODEL")
      "anthropic/claude-sonnet-4-20250514"))

;; ════════════════════════════════════════════════════════════════
;; Утилиты чтения файлов
;; ════════════════════════════════════════════════════════════════

(defun читать-файл (путь)
  "Прочитать файл целиком в строку."
  (with-open-file (f путь :external-format :utf-8)
    (let ((buf (make-string (file-length f))))
      (read-sequence buf f)
      ;; file-length может вернуть больше чем реальных символов для UTF-8
      (string-right-trim '(#\Nul) buf))))

(defun разбить-строки (текст)
  (uiop:split-string текст :separator '(#\Newline)))

(defun trim (s)
  (string-trim '(#\Space #\Newline #\Return #\Tab) s))

;; ════════════════════════════════════════════════════════════════
;; Индексация репозитория
;; ════════════════════════════════════════════════════════════════

(defstruct файл-инфо
  "Информация о файле: путь + список определений (имя . строка)."
  путь
  определения)

(defun собрать-файлы (каталог &optional (расширения '("py" "lisp" "js" "ts" "rb" "go" "java")))
  "Рекурсивно собрать файлы с заданными расширениями."
  (let ((результат '()))
    (dolist (расш расширения)
      (let ((паттерн (merge-pathnames
                      (make-pathname :directory '(:relative :wild-inferiors)
                                     :name :wild :type расш)
                      (uiop:ensure-directory-pathname каталог))))
        (dolist (ф (directory паттерн))
          (push (namestring ф) результат))))
    (sort результат #'string<)))

(defun извлечь-определения (путь)
  "Извлечь def/class/function определения с номерами строк.
   Поддерживает Python (def/class), JS/TS (function/class), Lisp (defun/defclass)."
  (handler-case
      (let ((строки (разбить-строки (читать-файл путь)))
            (результат '()))
        (loop for строка in строки
              for номер from 1
              do (let ((имя nil))
                   ;; Python: def xxx / class xxx
                   (cl-ppcre:register-groups-bind (совпадение)
                       ("^\\s*(?:def|class)\\s+(\\w+)" строка)
                     (setf имя совпадение))
                   ;; JS/TS: function xxx / class xxx
                   (unless имя
                     (cl-ppcre:register-groups-bind (совпадение)
                         ("^\\s*(?:export\\s+)?(?:async\\s+)?(?:function|class)\\s+(\\w+)" строка)
                       (setf имя совпадение)))
                   ;; Lisp: (defun xxx / (defclass xxx / (defmethod xxx
                   (unless имя
                     (cl-ppcre:register-groups-bind (совпадение)
                         ("^\\s*\\((?:defun|defclass|defmethod|defgeneric)\\s+(\\S+)" строка)
                       (setf имя совпадение)))
                   ;; Go: func xxx
                   (unless имя
                     (cl-ppcre:register-groups-bind (совпадение)
                         ("^func\\s+(?:\\([^)]+\\)\\s+)?(\\w+)" строка)
                       (setf имя совпадение)))
                   (when имя
                     (push (cons имя номер) результат))))
        (nreverse результат))
    (error () nil)))

(defun индексировать-репо (каталог)
  "Индексация репозитория: список файл-инфо с определениями.
   Возвращает список структур файл-инфо."
  (let ((файлы (собрать-файлы каталог)))
    (loop for ф in файлы
          for опр = (извлечь-определения ф)
          collect (make-файл-инфо :путь ф :определения опр))))

;; ════════════════════════════════════════════════════════════════
;; Парсер Mermaid-графа (переиспользуем из граф-аналитика)
;; ════════════════════════════════════════════════════════════════

(defstruct узел
  id метка ошибка? терминал? решение?)

(defstruct ребро
  откуда куда условие)

(defun ошибочный? (id метка)
  (or (cl-ppcre:scan "(?i)ERR" id)
      (cl-ppcre:scan "(?i)(ошибк|компенсац)" метка)))

(defun терминальный? (метка)
  (cl-ppcre:scan "(?i)(финиш|completed|OK\\d|успех|finish)" метка))

(defun разобрать-граф (текст)
  "Mermaid flowchart → (values hash-узлы список-рёбер)."
  (let ((узлы (make-hash-table :test #'equal))
        (рёбра '()))
    (cl-ppcre:do-register-groups (id метка)
        ("(\\w+)\\[\"([^\"]+)\"\\]" текст)
      (setf (gethash id узлы)
            (make-узел :id id :метка метка
                       :ошибка? (ошибочный? id метка)
                       :терминал? (терминальный? метка))))
    (cl-ppcre:do-register-groups (id метка)
        ("(\\w+)\\{\"([^\"]+)\"\\}" текст)
      (setf (gethash id узлы)
            (make-узел :id id :метка метка :решение? t)))
    (cl-ppcre:do-register-groups (откуда условие куда)
        ("(\\w+)\\s*-->(?:\\|\"([^\"]*)\"\\|)?\\s*(\\w+)" текст)
      (push (make-ребро :откуда откуда
                        :куда куда
                        :условие (when (and условие (plusp (length условие)))
                                   условие))
            рёбра))
    (values узлы (nreverse рёбра))))

;; ════════════════════════════════════════════════════════════════
;; JSON-схема tools для OpenRouter function calling
;; ════════════════════════════════════════════════════════════════

(defparameter *tools-schema*
  '(((:type . "function")
     (:function
      . ((:name . "search_in_file")
         (:description . "Search for a regex pattern in a file. Returns matching lines with line numbers.")
         (:parameters
          . ((:type . "object")
             (:properties
              . ((:path . ((:type . "string")
                           (:description . "File path relative to repository root")))
                 (:pattern . ((:type . "string")
                              (:description . "Regex pattern to search for")))))
             (:required "path" "pattern"))))))

    ((:type . "function")
     (:function
      . ((:name . "read_lines")
         (:description . "Read lines from a file by line number range.")
         (:parameters
          . ((:type . "object")
             (:properties
              . ((:path . ((:type . "string")
                           (:description . "File path relative to repository root")))
                 (:start . ((:type . "integer")
                            (:description . "Starting line number (1-based)")))
                 (:end . ((:type . "integer")
                          (:description . "Ending line number (inclusive)")))))
             (:required "path" "start" "end"))))))

    ((:type . "function")
     (:function
      . ((:name . "find_function")
         (:description . "Find a function or class definition by name. Returns file path and line number.")
         (:parameters
          . ((:type . "object")
             (:properties
              . ((:name . ((:type . "string")
                           (:description . "Function or class name to find")))))
             (:required "name"))))))

    ((:type . "function")
     (:function
      . ((:name . "bfs_predecessors")
         (:description . "Find nodes that lead to the given node using BFS backward traversal.")
         (:parameters
          . ((:type . "object")
             (:properties
              . ((:node_id . ((:type . "string")
                              (:description . "Target node ID")))
                 (:depth . ((:type . "integer")
                            (:description . "Maximum BFS depth (default 3)")))))
             (:required "node_id"))))))

    ((:type . "function")
     (:function
      . ((:name . "bfs_successors")
         (:description . "Find nodes reachable from the given node using BFS forward traversal.")
         (:parameters
          . ((:type . "object")
             (:properties
              . ((:node_id . ((:type . "string")
                              (:description . "Source node ID")))
                 (:depth . ((:type . "integer")
                            (:description . "Maximum BFS depth (default 3)")))))
             (:required "node_id"))))))

    ((:type . "function")
     (:function
      . ((:name . "get_node_label")
         (:description . "Get the label text of a graph node by its ID.")
         (:parameters
          . ((:type . "object")
             (:properties
              . ((:node_id . ((:type . "string")
                              (:description . "Node ID to look up")))))
             (:required "node_id"))))))

    ((:type . "function")
     (:function
      . ((:name . "report_finding")
         (:description . "Report an analysis finding. Call this for each issue discovered.")
         (:parameters
          . ((:type . "object")
             (:properties
              . ((:node . ((:type . "string")
                           (:description . "Related graph node ID")))
                 (:category . ((:type . "string")
                               (:description . "Finding category: compensation_gap, edge_case, unreachable, race_condition, missing_validation, etc.")))
                 (:description . ((:type . "string")
                                  (:description . "What the problem is")))
                 (:code_ref . ((:type . "string")
                               (:description . "File path and line reference, e.g. path/to/file.py:42")))
                 (:severity . ((:type . "string")
                               (:enum "critical" "warning" "info")
                               (:description . "Severity: critical (blocker), warning (should fix), info (note)")))
                 (:recommendation . ((:type . "string")
                                     (:description . "How to fix the issue")))))
             (:required "node" "category" "description" "severity"))))))

    ((:type . "function")
     (:function
      . ((:name . "finish")
         (:description . "Complete the analysis session. Call when done investigating.")
         (:parameters
          . ((:type . "object")
             (:properties
              . ((:summary . ((:type . "string")
                              (:description . "Brief summary of the analysis performed")))))
             (:required "summary"))))))))

;; ════════════════════════════════════════════════════════════════
;; Тесты
;; ════════════════════════════════════════════════════════════════

(defvar *тест-ошибок* 0)

(defmacro проверить (описание &body тело)
  `(handler-case
       (progn ,@тело)
     (error (e)
       (format t "FAIL: ~A — ~A~%" ,описание e)
       (incf *тест-ошибок*))))

(defvar *мой-путь* (or *load-pathname* *load-truename*
                       #P"мастер/потоки/агент-аналитик.lisp"))

(defun тест-индексация ()
  "Тест индексации на самом файле агент-аналитик.lisp."
  (let* ((мой-путь (merge-pathnames *мой-путь* (uiop:getcwd)))
         (каталог (directory-namestring мой-путь)))
    ;; собрать-файлы должен найти .lisp файлы
    (проверить "собрать-файлы находит lisp файлы"
      (let ((файлы (собрать-файлы каталог '("lisp"))))
        (assert (plusp (length файлы)) ()
                "Не найдено ни одного .lisp файла в ~A" каталог)))

    ;; извлечь-определения должен найти defun/defstruct
    (проверить "извлечь-определения находит определения"
      (let ((опр (извлечь-определения (namestring мой-путь))))
        (assert (plusp (length опр)) ()
                "Не найдено определений в агент-аналитик.lisp")
        ;; должен найти функцию ключ-апи
        (assert (find "ключ-апи" опр :key #'car :test #'string=) ()
                "Не найдена функция ключ-апи")))

    ;; индексировать-репо должен вернуть список файл-инфо
    (проверить "индексировать-репо возвращает список файл-инфо"
      (let ((индекс (индексировать-репо каталог)))
        (assert (plusp (length индекс)) ()
                "Пустой индекс для ~A" каталог)
        (assert (файл-инфо-p (first индекс)) ()
                "Элемент не файл-инфо")))))

(defun тест-граф-парсер ()
  "Тест парсера mermaid-графа."
  (let ((тест-граф "flowchart TD
A0[\"Создать заказ\"]
A1[\"Оплатить\"]
A2{\"Оплата OK?\"}
ERR1[\"Ошибка оплаты\"]
OK1[\"Заказ завершён\"]
A0 --> A1
A1 --> A2
A2 -->|\"Да\"| OK1
A2 -->|\"Нет\"| ERR1"))
    (multiple-value-bind (узлы рёбра) (разобрать-граф тест-граф)
      (проверить "парсер находит узлы"
        (assert (= (hash-table-count узлы) 5) ()
                "Ожидалось 5 узлов, получено ~A" (hash-table-count узлы)))
      (проверить "парсер находит рёбра"
        (assert (= (length рёбра) 4) ()
                "Ожидалось 4 ребра, получено ~A" (length рёбра)))
      (проверить "парсер определяет error-узлы"
        (let ((err1 (gethash "ERR1" узлы)))
          (assert err1 () "ERR1 не найден")
          (assert (узел-ошибка? err1) () "ERR1 не помечен как ошибка")))
      (проверить "парсер определяет decision-узлы"
        (let ((a2 (gethash "A2" узлы)))
          (assert a2 () "A2 не найден")
          (assert (узел-решение? a2) () "A2 не помечен как решение")))
      (проверить "рёбра имеют условия"
        (let ((с-условием (find-if #'ребро-условие рёбра)))
          (assert с-условием () "Не найдено ребро с условием"))))))

(defun тест-tools-schema ()
  "Проверить формат tools-schema."
  (проверить "tools-schema — список из 8 элементов"
    (assert (= (length *tools-schema*) 8) ()
            "Ожидалось 8 tools, получено ~A" (length *tools-schema*)))
  (проверить "каждый tool имеет type=function и function.name"
    (dolist (tool *tools-schema*)
      (assert (string= (cdr (assoc :type tool)) "function") ()
              "tool.type не function")
      (let ((fn (cdr (assoc :function tool))))
        (assert fn () "tool.function отсутствует")
        (assert (cdr (assoc :name fn)) () "tool.function.name отсутствует"))))
  (проверить "tools-schema сериализуется в валидный JSON"
    (let ((json (cl-json:encode-json-to-string *tools-schema*)))
      (assert (plusp (length json)) ()
              "JSON сериализация пустая")
      ;; проверить что парсится обратно
      (let ((parsed (cl-json:decode-json-from-string json)))
        (assert (= (length parsed) 8) ()
                "Обратный парсинг: ожидалось 8 элементов")))))

(defun тест-всё ()
  "Запуск всех тестов."
  (setf *тест-ошибок* 0)
  (format t "~%=== Тесты агент-аналитика ===~%")
  (тест-индексация)
  (тест-граф-парсер)
  (тест-tools-schema)
  (format t "~%Ошибок: ~A~%" *тест-ошибок*)
  (when (plusp *тест-ошибок*)
    (error "~A тестов провалено" *тест-ошибок*))
  (format t "Все тесты пройдены.~%"))

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
;; Tool implementations — код-навигация
;; ════════════════════════════════════════════════════════════════

(defun tool-search-in-file (репо-путь путь паттерн)
  "Поиск regex-паттерна в файле. Возвращает строку с совпадениями (номер: текст).
   ПУТЬ — относительный путь от корня репо. ПАТТЕРН — regex."
  (let* ((полный-путь (merge-pathnames путь (uiop:ensure-directory-pathname репо-путь)))
         (содержимое (handler-case (читать-файл (namestring полный-путь))
                       (error () (return-from tool-search-in-file
                                   (format nil "ERROR: File not found: ~A" путь)))))
         (строки (разбить-строки содержимое))
         (scanner (handler-case (cl-ppcre:create-scanner паттерн)
                    (error () (return-from tool-search-in-file
                                (format nil "ERROR: Invalid regex: ~A" паттерн)))))
         (результат '()))
    (loop for строка in строки
          for номер from 1
          when (cl-ppcre:scan scanner строка)
            do (push (format nil "~D: ~A" номер строка) результат))
    (if результат
        (format nil "~{~A~^~%~}" (nreverse результат))
        (format nil "No matches for pattern '~A' in ~A" паттерн путь))))

(defun tool-read-lines (репо-путь путь начало конец)
  "Чтение строк с НАЧАЛО по КОНЕЦ (включительно, 1-based) из файла.
   ПУТЬ — относительный путь от корня репо."
  (let* ((полный-путь (merge-pathnames путь (uiop:ensure-directory-pathname репо-путь)))
         (содержимое (handler-case (читать-файл (namestring полный-путь))
                       (error () (return-from tool-read-lines
                                   (format nil "ERROR: File not found: ~A" путь)))))
         (строки (разбить-строки содержимое))
         (всего (length строки))
         (н (max 1 (min начало всего)))
         (к (max н (min конец всего)))
         (результат '()))
    (loop for i from н to к
          for строка = (nth (1- i) строки)
          do (push (format nil "~D: ~A" i строка) результат))
    (if результат
        (format nil "~{~A~^~%~}" (nreverse результат))
        (format nil "ERROR: Invalid line range ~D-~D (file has ~D lines)" начало конец всего))))

(defun tool-find-function (индекс имя)
  "Найти определение функции/класса по имени в индексе репо.
   ИНДЕКС — результат индексировать-репо, ИМЯ — имя для поиска.
   Возвращает строку с найденными местами."
  (let ((результат '()))
    (dolist (фи индекс)
      (dolist (опр (файл-инфо-определения фи))
        (when (search (string-downcase имя)
                      (string-downcase (car опр)))
          (push (format nil "~A:~D (~A)"
                        (файл-инфо-путь фи) (cdr опр) (car опр))
                результат))))
    (if результат
        (format nil "~{~A~^~%~}" (nreverse результат))
        (format nil "No definition found for '~A'" имя))))

;; ════════════════════════════════════════════════════════════════
;; Tool implementations — граф-навигация
;; ════════════════════════════════════════════════════════════════

(defun tool-bfs-predecessors (узлы рёбра node-id &optional (глубина 3))
  "BFS назад по рёбрам графа: кто ведёт к NODE-ID до заданной ГЛУБИНЫ.
   Возвращает строку с найденными предшественниками."
  (unless (gethash node-id узлы)
    (return-from tool-bfs-predecessors
      (format nil "ERROR: Node '~A' not found in graph" node-id)))
  (let ((обратный (make-hash-table :test #'equal))
        (посещены (make-hash-table :test #'equal))
        (результат '()))
    ;; Строим обратный граф
    (dolist (р рёбра)
      (push (ребро-откуда р) (gethash (ребро-куда р) обратный)))
    (setf (gethash node-id посещены) t)
    (let ((очередь (list node-id)))
      (dotimes (_ глубина)
        (let ((следующий '()))
          (dolist (cur очередь)
            (dolist (prev (gethash cur обратный))
              (unless (gethash prev посещены)
                (setf (gethash prev посещены) t)
                (let ((у (gethash prev узлы)))
                  (push (if у
                            (format nil "~A[~A]" prev (узел-метка у))
                            prev)
                        результат))
                (push prev следующий))))
          (setf очередь следующий))))
    (if результат
        (format nil "Predecessors of ~A (depth ~D): ~{~A~^, ~}"
                node-id глубина (nreverse результат))
        (format nil "No predecessors found for ~A within depth ~D"
                node-id глубина))))

(defun tool-bfs-successors (узлы рёбра node-id &optional (глубина 3))
  "BFS вперёд по рёбрам графа: куда ведёт NODE-ID до заданной ГЛУБИНЫ.
   Возвращает строку с найденными последователями."
  (unless (gethash node-id узлы)
    (return-from tool-bfs-successors
      (format nil "ERROR: Node '~A' not found in graph" node-id)))
  (let ((смежность (make-hash-table :test #'equal))
        (посещены (make-hash-table :test #'equal))
        (результат '()))
    ;; Строим прямой граф
    (dolist (р рёбра)
      (push (ребро-куда р) (gethash (ребро-откуда р) смежность)))
    (setf (gethash node-id посещены) t)
    (let ((очередь (list node-id)))
      (dotimes (_ глубина)
        (let ((следующий '()))
          (dolist (cur очередь)
            (dolist (next (gethash cur смежность))
              (unless (gethash next посещены)
                (setf (gethash next посещены) t)
                (let ((у (gethash next узлы)))
                  (push (if у
                            (format nil "~A[~A]" next (узел-метка у))
                            next)
                        результат))
                (push next следующий))))
          (setf очередь следующий))))
    (if результат
        (format nil "Successors of ~A (depth ~D): ~{~A~^, ~}"
                node-id глубина (nreverse результат))
        (format nil "No successors found for ~A within depth ~D"
                node-id глубина))))

(defun tool-get-node-label (узлы node-id)
  "Получить метку узла по ID. Возвращает строку."
  (let ((у (gethash node-id узлы)))
    (if у
        (format nil "~A: ~A" node-id (узел-метка у))
        (format nil "ERROR: Node '~A' not found in graph" node-id))))

;; ════════════════════════════════════════════════════════════════
;; Tool implementations — управление
;; ════════════════════════════════════════════════════════════════

(defun tool-report-finding (findings node category description severity
                            &optional code-ref recommendation)
  "Добавить finding в список. Мутирует FINDINGS (список с fill-pointer).
   Возвращает строку-подтверждение."
  (let ((finding (list (cons :node node)
                       (cons :category category)
                       (cons :description description)
                       (cons :severity severity))))
    (when code-ref
      (push (cons :code-ref code-ref) finding))
    (when recommendation
      (push (cons :recommendation recommendation) finding))
    (vector-push-extend (nreverse finding) findings)
    (format nil "Finding recorded (#~D, ~A, ~A): ~A"
            (length findings) severity node description)))

(defun tool-finish (findings summary)
  "Маркер завершения анализа. Возвращает alist с :finish t и summary."
  (list (cons :finish t)
        (cons :summary summary)
        (cons :findings-count (length findings))))

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

(defun тест-tool-search-in-file ()
  "Тест tool-search-in-file на текущем файле."
  (let ((репо-путь (directory-namestring
                     (merge-pathnames *мой-путь* (uiop:getcwd))))
        (отн-путь (file-namestring
                    (merge-pathnames *мой-путь* (uiop:getcwd)))))
    ;; Поиск существующей функции
    (проверить "search-in-file находит defun"
      (let ((рез (tool-search-in-file репо-путь отн-путь "defun ключ-апи")))
        (assert (search "ключ-апи" рез) ()
                "Не найден defun ключ-апи: ~A" рез)))
    ;; Поиск несуществующего паттерна (anchored — не совпадёт с самим собой в тексте)
    (проверить "search-in-file — нет совпадений"
      (let ((рез (tool-search-in-file репо-путь отн-путь "^ZZZNOMATCH$")))
        (assert (search "No matches" рез) ()
                "Ожидалось 'No matches': ~A" рез)))
    ;; Ошибка: несуществующий файл
    (проверить "search-in-file — несуществующий файл"
      (let ((рез (tool-search-in-file репо-путь "nonexistent.py" "test")))
        (assert (search "ERROR" рез) ()
                "Ожидалась ошибка: ~A" рез)))
    ;; Ошибка: невалидный regex
    (проверить "search-in-file — невалидный regex"
      (let ((рез (tool-search-in-file репо-путь отн-путь "[invalid")))
        (assert (search "ERROR" рез) ()
                "Ожидалась ошибка regex: ~A" рез)))))

(defun тест-tool-read-lines ()
  "Тест tool-read-lines на текущем файле."
  (let ((репо-путь (directory-namestring
                     (merge-pathnames *мой-путь* (uiop:getcwd))))
        (отн-путь (file-namestring
                    (merge-pathnames *мой-путь* (uiop:getcwd)))))
    ;; Чтение первых 3 строк
    (проверить "read-lines — первые строки"
      (let ((рез (tool-read-lines репо-путь отн-путь 1 3)))
        (assert (search "1:" рез) ()
                "Не найдена строка 1: ~A" рез)
        (assert (search "3:" рез) ()
                "Не найдена строка 3: ~A" рез)))
    ;; Чтение одной строки
    (проверить "read-lines — одна строка"
      (let ((рез (tool-read-lines репо-путь отн-путь 1 1)))
        (assert (search "1:" рез) ()
                "Не найдена строка 1: ~A" рез)
        ;; Не должно быть строки 2
        (assert (not (search "2:" рез)) ()
                "Лишняя строка 2: ~A" рез)))
    ;; Несуществующий файл
    (проверить "read-lines — несуществующий файл"
      (let ((рез (tool-read-lines репо-путь "nonexistent.py" 1 5)))
        (assert (search "ERROR" рез) ()
                "Ожидалась ошибка: ~A" рез)))))

(defun тест-tool-find-function ()
  "Тест tool-find-function на индексе текущего каталога."
  (let* ((мой-абс (merge-pathnames *мой-путь* (uiop:getcwd)))
         (каталог (directory-namestring мой-абс))
         (индекс (индексировать-репо каталог)))
    ;; Поиск существующей функции
    (проверить "find-function находит ключ-апи"
      (let ((рез (tool-find-function индекс "ключ-апи")))
        (assert (search "ключ-апи" рез) ()
                "Не найдена ключ-апи: ~A" рез)
        (assert (search ".lisp:" рез) ()
                "Нет пути к файлу: ~A" рез)))
    ;; Частичное совпадение имени
    (проверить "find-function — частичное совпадение"
      (let ((рез (tool-find-function индекс "tool-search")))
        (assert (search "tool-search-in-file" рез) ()
                "Не найдена tool-search-in-file: ~A" рез)))
    ;; Несуществующая функция (имя не встречается ни в одном определении)
    (проверить "find-function — не найдена"
      (let ((рез (tool-find-function индекс "qqq_нет_такой_999")))
        (assert (search "No definition" рез) ()
                "Ожидалось 'No definition': ~A" рез)))))

(defun тест-tool-bfs-predecessors ()
  "Тест tool-bfs-predecessors на тестовом графе."
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
      ;; BFS назад от ERR1 — должен найти A2, A1, A0
      (проверить "bfs-predecessors от ERR1 глубина 3"
        (let ((рез (tool-bfs-predecessors узлы рёбра "ERR1" 3)))
          (assert (search "A2" рез) ()
                  "Не найден A2 в предшественниках ERR1: ~A" рез)
          (assert (search "A1" рез) ()
                  "Не найден A1 в предшественниках ERR1: ~A" рез)
          (assert (search "A0" рез) ()
                  "Не найден A0 в предшественниках ERR1: ~A" рез)))
      ;; BFS назад от ERR1 глубина 1 — только A2
      (проверить "bfs-predecessors от ERR1 глубина 1"
        (let ((рез (tool-bfs-predecessors узлы рёбра "ERR1" 1)))
          (assert (search "A2" рез) ()
                  "Не найден A2: ~A" рез)
          (assert (not (search "A0" рез)) ()
                  "A0 не должен быть на глубине 1: ~A" рез)))
      ;; BFS назад от A0 — нет предшественников
      (проверить "bfs-predecessors от A0 — нет предшественников"
        (let ((рез (tool-bfs-predecessors узлы рёбра "A0")))
          (assert (search "No predecessors" рез) ()
                  "Ожидалось 'No predecessors': ~A" рез)))
      ;; Несуществующий узел
      (проверить "bfs-predecessors — несуществующий узел"
        (let ((рез (tool-bfs-predecessors узлы рёбра "NOSUCH")))
          (assert (search "ERROR" рез) ()
                  "Ожидалась ошибка: ~A" рез))))))

(defun тест-tool-bfs-successors ()
  "Тест tool-bfs-successors на тестовом графе."
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
      ;; BFS вперёд от A0 — должен найти A1, A2, ERR1, OK1
      (проверить "bfs-successors от A0 глубина 3"
        (let ((рез (tool-bfs-successors узлы рёбра "A0" 3)))
          (assert (search "A1" рез) ()
                  "Не найден A1: ~A" рез)
          (assert (search "A2" рез) ()
                  "Не найден A2: ~A" рез)))
      ;; BFS вперёд от A0 глубина 1 — только A1
      (проверить "bfs-successors от A0 глубина 1"
        (let ((рез (tool-bfs-successors узлы рёбра "A0" 1)))
          (assert (search "A1" рез) ()
                  "Не найден A1: ~A" рез)
          (assert (not (search "ERR1" рез)) ()
                  "ERR1 не должен быть на глубине 1: ~A" рез)))
      ;; BFS вперёд от ERR1 — нет последователей
      (проверить "bfs-successors от ERR1 — нет последователей"
        (let ((рез (tool-bfs-successors узлы рёбра "ERR1")))
          (assert (search "No successors" рез) ()
                  "Ожидалось 'No successors': ~A" рез)))
      ;; Несуществующий узел
      (проверить "bfs-successors — несуществующий узел"
        (let ((рез (tool-bfs-successors узлы рёбра "NOSUCH")))
          (assert (search "ERROR" рез) ()
                  "Ожидалась ошибка: ~A" рез))))))

(defun тест-tool-get-node-label ()
  "Тест tool-get-node-label."
  (let ((тест-граф "flowchart TD
A0[\"Создать заказ\"]
ERR1[\"Ошибка оплаты\"]
A0 --> ERR1"))
    (multiple-value-bind (узлы рёбра) (разобрать-граф тест-граф)
      (declare (ignore рёбра))
      ;; Существующий узел
      (проверить "get-node-label — существующий узел"
        (let ((рез (tool-get-node-label узлы "A0")))
          (assert (search "Создать заказ" рез) ()
                  "Не найдена метка: ~A" рез)))
      ;; Несуществующий узел
      (проверить "get-node-label — несуществующий узел"
        (let ((рез (tool-get-node-label узлы "NOSUCH")))
          (assert (search "ERROR" рез) ()
                  "Ожидалась ошибка: ~A" рез))))))

(defun тест-tool-report-finding ()
  "Тест tool-report-finding."
  (let ((findings (make-array 0 :adjustable t :fill-pointer 0)))
    ;; Добавить finding
    (проверить "report-finding — добавление"
      (let ((рез (tool-report-finding findings "ERR1" "compensation_gap"
                                      "Order not rolled back" "critical"
                                      "orders/services.py:48"
                                      "Add order.delete()")))
        (assert (search "Finding recorded" рез) ()
                "Ожидалось подтверждение: ~A" рез)
        (assert (= (length findings) 1) ()
                "Ожидался 1 finding, получено ~A" (length findings))))
    ;; Проверить содержимое finding
    (проверить "report-finding — содержимое"
      (let ((f (aref findings 0)))
        (assert (string= (cdr (assoc :node f)) "ERR1") ()
                "node не ERR1")
        (assert (string= (cdr (assoc :severity f)) "critical") ()
                "severity не critical")))
    ;; Добавить второй finding без optional полей
    (проверить "report-finding — без optional полей"
      (tool-report-finding findings "A2" "edge_case"
                           "Branch not covered" "warning")
      (assert (= (length findings) 2) ()
              "Ожидалось 2 findings, получено ~A" (length findings)))))

(defun тест-tool-finish ()
  "Тест tool-finish."
  (let ((findings (make-array 2 :adjustable t :fill-pointer 2)))
    (проверить "finish — возвращает alist с :finish t"
      (let ((рез (tool-finish findings "Analysis complete")))
        (assert (cdr (assoc :finish рез)) ()
                ":finish не t: ~A" рез)
        (assert (string= (cdr (assoc :summary рез)) "Analysis complete") ()
                "summary не совпадает: ~A" рез)
        (assert (= (cdr (assoc :findings-count рез)) 2) ()
                "findings-count не 2: ~A" рез)))))

(defun тест-всё ()
  "Запуск всех тестов."
  (setf *тест-ошибок* 0)
  (format t "~%=== Тесты агент-аналитика ===~%")
  (тест-индексация)
  (тест-граф-парсер)
  (тест-tools-schema)
  (тест-tool-search-in-file)
  (тест-tool-read-lines)
  (тест-tool-find-function)
  (тест-tool-bfs-predecessors)
  (тест-tool-bfs-successors)
  (тест-tool-get-node-label)
  (тест-tool-report-finding)
  (тест-tool-finish)
  (format t "~%Ошибок: ~A~%" *тест-ошибок*)
  (when (plusp *тест-ошибок*)
    (error "~A тестов провалено" *тест-ошибок*))
  (format t "Все тесты пройдены.~%"))

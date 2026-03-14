# План: Агент-аналитик кода

Агентная версия граф-аналитика. LLM сама строит план исследования через tool calls.

## Архитектура

```
┌─────────────────────────────────────────────────────┐
│                  запустить-агент                    │
│   вход: mermaid-граф + путь-к-репо                 │
├─────────────────────────────────────────────────────┤
│  1. разобрать-граф (существующий)                  │
│  2. индексировать-репо → кэш файлов/функций        │
│  3. tool-loop ↓                                    │
├─────────────────────────────────────────────────────┤
│  ┌───────────────┐     ┌──────────────────────┐    │
│  │   вызвать-llm │ ←→  │  обработать-tools    │    │
│  │  (tool_use)   │     │  (dispatch по имени) │    │
│  └───────────────┘     └──────────────────────┘    │
│         ↑ assistant message          ↓ results     │
│         └────────────────────────────┘             │
│                      ↓ finish                      │
├─────────────────────────────────────────────────────┤
│  собрать-отчёт (findings → markdown)               │
└─────────────────────────────────────────────────────┘
```

## Tool-call loop

```lisp
(defun tool-loop (сообщения узлы рёбра репо findings &optional (счётчик 0))
  "Рекурсивный loop до finish или лимита."
  (when (>= счётчик 30)
    (return-from tool-loop findings))
  
  (let* ((ответ (вызвать-llm-tools сообщения))
         (tool-calls (извлечь-tool-calls ответ)))
    
    (unless tool-calls
      ;; Нет tool calls → агент закончил текстом
      (return-from tool-loop findings))
    
    (let ((результаты (mapcar 
                        (lambda (tc)
                          (выполнить-tool tc узлы рёбра репо findings))
                        tool-calls)))
      
      ;; finish? → выход
      (when (найти-finish результаты)
        (return-from tool-loop findings))
      
      ;; Добавить assistant message + tool results в историю
      (tool-loop 
        (append сообщения 
                (list (assistant-msg ответ))
                (tool-result-msgs tool-calls результаты))
        узлы рёбра репо findings
        (+ счётчик (length tool-calls))))))
```

## Формат tool_use

Запрос к LLM:

```json
{
  "model": "...",
  "messages": [...],
  "tools": [
    {
      "type": "function",
      "function": {
        "name": "search_in_file",
        "description": "grep по файлу",
        "parameters": {
          "type": "object",
          "properties": {
            "path": {"type": "string"},
            "pattern": {"type": "string"}
          },
          "required": ["path", "pattern"]
        }
      }
    }
  ]
}
```

Ответ с tool_calls:

```json
{
  "choices": [{
    "message": {
      "role": "assistant",
      "tool_calls": [
        {
          "id": "call_abc123",
          "type": "function",
          "function": {
            "name": "search_in_file",
            "arguments": "{\"path\": \"views.py\", \"pattern\": \"def create\"}"
          }
        }
      ]
    }
  }]
}
```

Tool result обратно:

```json
{
  "role": "tool",
  "tool_call_id": "call_abc123",
  "content": "42: def create_order(request):\n43:     order = Order.objects.create(...)"
}
```

## Инструменты

### Код-навигация

| Tool | Что делает | Реализация |
|------|-----------|------------|
| `search_in_file` | grep паттерн в файле | `cl-ppcre:scan` по строкам |
| `read_lines` | строки N-M | `subseq` + кэш файлов |
| `find_function` | функция/метод по имени | ripgrep или regex по репо |

### Граф-навигация

| Tool | Что делает | Реализация |
|------|-----------|------------|
| `bfs_predecessors` | кто ведёт к узлу | существующий BFS из граф-аналитика |
| `bfs_successors` | куда ведёт узел | симметричный BFS |
| `get_node_label` | метка узла | lookup в hash-table |

### Управление

| Tool | Что делает |
|------|-----------|
| `report_finding` | добавить находку в список |
| `finish` | завершить сессию, вернуть summary |

## Системный промпт

```
Ты — агент-аналитик кода. Твоя задача: найти проблемы в реализации процесса.

ВХОД:
- Граф процесса (Mermaid) — какие шаги и переходы
- Путь к репозиторию — где лежит код

ИНСТРУМЕНТЫ:
- search_in_file, read_lines, find_function — чтение кода
- bfs_predecessors, bfs_successors, get_node_label — навигация по графу
- report_finding — зафиксировать проблему
- finish — завершить анализ

СТРАТЕГИЯ:
1. Начни с error-узлов (ERR*) и проверь их компенсацию
2. Для каждого error-узла найди предшественников через bfs_predecessors
3. Найди код этих шагов через find_function и search_in_file
4. Проверь: что изменяется в БД до ошибки? Откатывается ли?
5. Зафиксируй находки через report_finding
6. Перейди к decision-узлам — все ли ветки покрыты?
7. Когда достаточно исследовал — вызови finish

ОГРАНИЧЕНИЯ:
- Максимум 30 tool calls. Планируй эффективно.
- Не выдумывай проблемы. Только то, что видишь в коде.
- Severity: critical (блокер), warning (надо поправить), info (замечание)

ФОРМАТ FINDING:
{
  "node": "ERR1",
  "category": "compensation_gap|edge_case|unreachable|...",
  "description": "что не так",
  "code_ref": "path/to/file.py:42",
  "severity": "critical|warning|info",
  "recommendation": "как исправить"
}
```

## Стратегия завершения

Агент вызывает `finish(summary)` когда:
- Исследовал все error-узлы и их предшественников
- Проверил ключевые decision points
- Осталось < 5 tool calls и ничего нового не находит

Принудительное завершение:
- 30 tool calls → собрать findings + warning о незавершённом анализе
- Ошибка API → вернуть partial results

## Индексация репо

Один раз при старте:

```lisp
(defun индексировать-репо (путь)
  "Кэш: список файлов + функций."
  (let ((файлы (directory (merge-pathnames "**/*.py" путь))))
    (loop for f in файлы
          collect (cons f (извлечь-функции f)))))

(defun извлечь-функции (путь)
  "def/class имена + номера строк."
  (loop for (строка номер) in (читать-с-номерами путь)
        when (cl-ppcre:scan "^\\s*(def|class)\\s+(\\w+)" строка)
        collect (cons (match-имя) номер)))
```

`find_function` делает lookup в этом кэше, не ripgrep каждый раз.

## Интеграция с граф-аналитиком

Переиспользовать:
- `разобрать-граф` — парсинг Mermaid
- `вызвать-llm` — обёртка над OpenRouter (добавить tools parameter)
- `отформатировать-отчёт` — финальный markdown
- структуры `узел`, `ребро`

Новое:
- `вызвать-llm-tools` — вариант с tools schema
- `выполнить-tool` — dispatcher
- `tool-loop` — рекурсивный loop
- `индексировать-репо` — кэш функций

## Структура файла

```
мастер/потоки/агент-аналитик.lisp

├── Конфигурация
│   └── *tools-schema* — JSON-схема всех инструментов
│
├── Tool implementations
│   ├── tool-search-in-file
│   ├── tool-read-lines
│   ├── tool-find-function
│   ├── tool-bfs-predecessors
│   ├── tool-bfs-successors
│   ├── tool-get-node-label
│   ├── tool-report-finding
│   └── tool-finish
│
├── Dispatcher
│   └── выполнить-tool (имя args) → result string
│
├── LLM
│   ├── вызвать-llm-tools (messages) → response
│   └── извлечь-tool-calls (response) → list
│
├── Loop
│   └── tool-loop (messages state) → findings
│
└── Entry
    └── выполнить (граф-путь репо-путь) → отчёт
```

## Пример сессии

```
User: [системный промпт + граф + начальное сообщение]

Agent: tool_call: bfs_predecessors("ERR1", 3)
Tool:  A2, A1, A0

Agent: tool_call: get_node_label("A2")  
Tool:  "Создать заказ в БД"

Agent: tool_call: find_function("create_order")
Tool:  orders/services.py:45

Agent: tool_call: read_lines("orders/services.py", 45, 70)
Tool:  [код создания заказа]

Agent: tool_call: bfs_predecessors("ERR1", 1)
       tool_call: search_in_file("orders/services.py", "delete|rollback")
Tool:  [предшественник: A3]
Tool:  [нет совпадений]

Agent: tool_call: report_finding(
         severity="critical",
         node="ERR1", 
         description="Order создаётся в A2, но при ошибке в A3 не удаляется",
         code_ref="orders/services.py:48",
         recommendation="Добавить order.delete() в обработчик ERR1"
       )
Tool:  OK

Agent: tool_call: finish("Проанализировал ERR1. Найдена 1 критическая проблема.")
```

## Задачи реализации

Валидация: `sbcl --load мастер/потоки/агент-аналитик.lisp --eval '(поток-агент-аналитик::тест-всё)' --quit`

---

### Task 1: Скелет пакета, конфигурация, индексация репо, схема tools

- [x] 1.1 Создать `мастер/потоки/агент-аналитик.lisp` — defpackage, quickload зависимостей
- [x] 1.2 Утилиты чтения файлов (читать-файл, разбить-строки) и конфигурация (ключ-апи, модель-апи)
- [x] 1.3 Индексация репо — кэш файлов + извлечение def/class определений с номерами строк
- [x] 1.4 JSON-схема всех 8 tools (*tools-schema*) в формате OpenRouter function calling
- [x] 1.5 Базовый тест — индексация тестового каталога, проверка формата схемы

---

### Task 2: Tool implementations — код-навигация

- [x] 2.1 `tool-search-in-file` — cl-ppcre:scan по строкам файла, возврат совпадений с номерами строк
- [x] 2.2 `tool-read-lines` — чтение строк N-M из файла (через кэш)
- [x] 2.3 `tool-find-function` — lookup в кэше индексации по имени функции/класса
- [x] 2.4 Тесты code-navigation tools на fixture-файлах

---

### Task 3: Tool implementations — граф-навигация + управление

- [x] 3.1 `tool-bfs-predecessors` — BFS назад по рёбрам графа до заданной глубины
- [x] 3.2 `tool-bfs-successors` — BFS вперёд по рёбрам графа до заданной глубины
- [x] 3.3 `tool-get-node-label` — получить метку узла по id
- [x] 3.4 `tool-report-finding` — добавить finding в список (мутация state)
- [x] 3.5 `tool-finish` — маркер завершения + summary
- [x] 3.6 Тесты graph + management tools

---

### Task 4: LLM с tool_use + dispatcher + tool-loop

- [x] 4.1 `вызвать-llm-tools` — POST с tools parameter, парсинг tool_calls из ответа
- [x] 4.2 `извлечь-tool-calls` — извлечение tool_calls из JSON-ответа OpenRouter
- [x] 4.3 `выполнить-tool` — dispatcher: имя → вызов соответствующей функции
- [x] 4.4 `tool-loop` — рекурсивный loop: LLM → extract → dispatch → append → repeat
- [x] 4.5 Тесты с mock LLM — dispatcher routing, loop termination

---

### Task 5: Системный промпт, точка входа, интеграция

- [x] 5.1 Системный промпт агента (стратегия анализа, формат findings, ограничения)
- [x] 5.2 `выполнить` — entry point: парсинг аргументов, граф + репо путь, запуск pipeline
- [x] 5.3 Сборка отчёта из findings (переиспользовать `отформатировать-отчёт` из граф-аналитика)
- [x] 5.4 Обработка ошибок и принудительное завершение (лимит calls, API errors)
- [x] 5.5 Интеграционный тест с mock LLM — полный pipeline от входа до отчёта

---

## Открытые вопросы

1. **Многофайловые транзакции** — как агенту передать что `@transaction.atomic` уже есть выше по стеку? Возможно: добавить tool `find_decorator_in_stack`.

2. **Большие репо** — кэш функций может быть большим. Lazy-load по директориям?

3. **Параллельные tool calls** — OpenRouter поддерживает, но усложняет loop. Пока sequential.

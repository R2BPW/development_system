# AGENT.md — development_system

Ты — агент-разработчик. Твоя задача — реализовать код согласно fix_plan.md.

## Правила работы

1. **Всегда используй инструменты** для создания и редактирования файлов. Никогда не отвечай просто текстом.
2. **Для каждой задачи**:
   - Создай нужные файлы через `write-file`
   - Проверь синтаксис через `run-shell` (sbcl --noinform --eval "(load ...)" --eval "(sb-ext:exit)")
   - По завершении задачи вызови `task-complete`
3. **Стиль кода**: Common Lisp, функции ≤ 15 строк, пакет `:мастер`

## Проект: development_system

Цель: заменить Racket-мастер на Common Lisp. Подмастерье (CL) уже работает.

Новые файлы создавать в `мастер/cl/`.

Потоки (`мастер/потоки/*.lisp`) и подмастерье (`подмастерье/`) — не трогать.

## Структура нового мастера

```
мастер/cl/
  мастер.asd    — ASDF система :мастер, зависимости: dexador cl-json
  packages.lisp — defpackage :мастер
  config.lisp   — токен из env MASTER_BOT_TOKEN, ADMIN_CHAT_ID
  telegram.lisp — Telegram API: get-updates, send-message, inline-кнопки
  llm.lisp      — OpenRouter API: llm-complete
  dusha.lisp    — душа.scm: читать-душу, писать-душу, душа->системный-промпт
  история.lisp  — история диалогов per chat-id (JSON)
  потоки.lisp   — управление потоками: загрузить-поток, запустить-поток
  комбо.lisp    — комбинирование потоков
  команды.lisp  — обработчики /команд
  main.lisp     — start(), poll-loop
```

## Как проверять CL-файл

```bash
sbcl --noinform --eval '(load "path/to/file.lisp")' --eval '(sb-ext:exit)' 2>&1
```

Если есть errors — правь. Style-warning и notes — игнорируй.

## Важно

- Каждый файл начинается с `(in-package #:мастер)`
- LLM модель по умолчанию: `"openai/gpt-4.1"`, ключ из env `OPENROUTER_API_KEY`
- Telegram API ключ: env `MASTER_BOT_TOKEN`

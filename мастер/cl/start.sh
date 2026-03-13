#!/usr/bin/env bash
# Запуск CL-мастера

export MASTER_BOT_TOKEN="${MASTER_BOT_TOKEN:?нужен MASTER_BOT_TOKEN}"
export ADMIN_CHAT_ID="${ADMIN_CHAT_ID:?нужен ADMIN_CHAT_ID}"
export OPENROUTER_API_KEY="${OPENROUTER_API_KEY:?нужен OPENROUTER_API_KEY}"

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

exec sbcl --noinform \
  --load ~/quicklisp/setup.lisp \
  --eval "(push #p\"${SCRIPT_DIR}/\" asdf:*central-registry*)" \
  --eval '(asdf:load-system :мастер :verbose nil)' \
  --eval '(мастер:start)'

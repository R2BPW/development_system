#!/usr/bin/env bash
# run.sh — запустить все тесты
# Использование:
#   ./тесты/run.sh               # всё
#   ./тесты/run.sh unit          # только юнит
#   ./тесты/run.sh streams       # только потоки
#   ./тесты/run.sh bot           # только бот (нужен MASTER_BOT_TOKEN)

set -euo pipefail

ПРОЕКТ="$(cd "$(dirname "$0")/.." && pwd)"
ТЕСТЫ="$ПРОЕКТ/тесты"

export OPENROUTER_API_KEY="${OPENROUTER_API_KEY:-}"
export MASTER_BOT_TOKEN="${MASTER_BOT_TOKEN:-}"
export ADMIN_CHAT_ID="${ADMIN_CHAT_ID:-250203350}"

TOTAL_FAIL=0
SUITE="${1:-all}"

run_suite() {
  local имя="$1" команда="$2"
  echo ""
  echo "┌─────────────────────────────────────────┐"
  echo "│  Suite: $имя"
  echo "└─────────────────────────────────────────┘"
  if eval "$команда"; then
    echo "→ ОК"
  else
    TOTAL_FAIL=$((TOTAL_FAIL + 1))
    echo "→ ПРОВАЛ"
  fi
}

case "$SUITE" in
  unit|all)
    run_suite "Юнит-тесты (Racket)" \
      "racket '$ТЕСТЫ/unit.rkt'"
    ;;&

  generation|all)
    run_suite "Тесты генерации потока (mock LLM)" \
      "racket '$ТЕСТЫ/generation.rkt'"
    ;;&

  streams|all)
    run_suite "Тесты потоков (SBCL)" \
      "bash '$ТЕСТЫ/streams.sh'"
    ;;&

  bot|all)
    if [[ -z "$MASTER_BOT_TOKEN" ]]; then
      echo ""
      echo "⚠️  Smoke-тесты бота пропущены (нет MASTER_BOT_TOKEN)"
    else
      run_suite "Smoke-тесты бота (Telegram)" \
        "bash '$ТЕСТЫ/bot.sh'"
    fi
    ;;
esac

echo ""
echo "══════════════════════════════════════════"
if [[ $TOTAL_FAIL -eq 0 ]]; then
  echo "  ВСЕ СЮИТЫ ПРОШЛИ ✅"
else
  echo "  ПРОВАЛИЛО СЮИТ: $TOTAL_FAIL ❌"
fi
echo "══════════════════════════════════════════"
echo ""

exit $TOTAL_FAIL

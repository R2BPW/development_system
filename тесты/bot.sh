#!/usr/bin/env bash
# bot.sh — smoke-тесты Telegram-ботов
# Отправляет команды через Bot API, проверяет ответы

set -euo pipefail

MASTER_TOKEN="${MASTER_BOT_TOKEN:-}"
ADMIN_ID="${ADMIN_CHAT_ID:-250203350}"
WAIT=12  # секунд ожидания ответа от бота

PASS=0; FAIL=0

ok()   { echo "  ✅ $1"; ((PASS++)); }
fail() { echo "  ❌ $1"; ((FAIL++)); }
skip() { echo "  ⚠️  $1 (пропущен)"; }

if [[ -z "$MASTER_TOKEN" ]]; then
  echo "Нужен MASTER_BOT_TOKEN"
  exit 1
fi

# Отправить сообщение боту
send() {
  local текст="$1"
  curl -s -X POST "https://api.telegram.org/bot${MASTER_TOKEN}/sendMessage" \
    -d "chat_id=${ADMIN_ID}" \
    -d "text=${текст}" > /dev/null
}

# Получить последний ответ бота за последние N секунд
last_reply() {
  python3 - "$MASTER_TOKEN" "$ADMIN_ID" << 'EOF'
import sys, json, urllib.request, time

token, chat_id = sys.argv[1], int(sys.argv[2])
url = f"https://api.telegram.org/bot{token}/getUpdates?limit=20"
r = json.loads(urllib.request.urlopen(url).read())
# Ищем последнее сообщение ОТ бота (не от пользователя)
for u in reversed(r.get("result", [])):
    msg = u.get("message", {})
    doc = u.get("message", {}).get("document")
    from_id = msg.get("from", {}).get("id")
    # Бот — не пользователь (is_bot = True)
    if msg.get("from", {}).get("is_bot") and msg.get("chat", {}).get("id") == chat_id:
        text = msg.get("text") or msg.get("caption") or ""
        fname = doc.get("file_name") if doc else None
        print(json.dumps({"text": text, "doc": fname}))
        sys.exit(0)
print(json.dumps({"text": "", "doc": None}))
EOF
}

# Ожидать ответа, содержащего паттерн
wait_for() {
  local паттерн="$1"
  for i in $(seq 1 $WAIT); do
    sleep 1
    ответ=$(last_reply)
    текст=$(echo "$ответ" | python3 -c "import sys,json; d=json.load(sys.stdin); print(d.get('text',''))")
    if echo "$текст" | grep -qi "$паттерн" 2>/dev/null; then
      echo "$ответ"
      return 0
    fi
  done
  return 1
}

echo ""
echo "══════════════════════════════════════════"
echo "  Smoke-тесты Мастера (Telegram)"
echo "══════════════════════════════════════════"

# ── /старт ───────────────────────────────────
echo ""
echo "▶ /старт → меню"
send "/старт"
if wait_for "Выберите\|действие\|время\|Породить" > /dev/null 2>&1; then
  ok "/старт вернул меню"
else
  fail "/старт не ответил"
fi

# ── /потоки ───────────────────────────────────
echo ""
echo "▶ /потоки → список потоков"
send "/потоки"
if wait_for "уточнение\|реакт\|питон\|Потоков нет" > /dev/null 2>&1; then
  ok "/потоки вернул список"
else
  fail "/потоки не ответил"
fi

# ── /состояние ───────────────────────────────
echo ""
echo "▶ /состояние → активный поток"
send "/состояние"
if wait_for "поток\|Поток\|активный\|Нет" > /dev/null 2>&1; then
  ok "/состояние вернул статус"
else
  fail "/состояние не ответил"
fi

# ── /запустить эхо ping ─────────────────────────
echo ""
echo "▶ /запустить эхо ping → Эхо: ping"
send "/запустить эхо ping"
if wait_for "Эхо.*ping\|эхо.*ping" > /dev/null 2>&1; then
  ok "/запустить эхо ping вернул результат"
else
  fail "/запустить эхо ping не ответил"
fi

# ── свободный текст ────────────────────────────
echo ""
echo "▶ свободный текст → LLM ответ"
send "скажи одно слово: привет"
WAIT=30
if wait_for "[а-яА-Яa-zA-Z]" > /dev/null 2>&1; then
  ok "свободный текст получил ответ от LLM"
else
  fail "свободный текст не получил ответа"
fi
WAIT=12

# ── итог ─────────────────────────────────────
echo ""
echo "══════════════════════════════════════════"
echo "  Итог: ✅ $PASS  ❌ $FAIL"
echo "══════════════════════════════════════════"
echo ""

exit $FAIL

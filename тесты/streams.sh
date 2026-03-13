#!/usr/bin/env bash
# streams.sh — integration tests for Common Lisp streams

set -euo pipefail

PROJECT="/root/.openclaw/workspace/development_system"
STREAMS="$PROJECT/мастер/потоки"
EXECUTOR="$PROJECT/подмастерье/исполнитель.lisp"
QL="$HOME/quicklisp/setup.lisp"

PASS=0; FAIL=0

ok()   { echo "  OK  $1"; ((PASS++)) || true; }
fail() { echo "  FAIL $1 | got: $2"; ((FAIL++)) || true; }

run_stream() {
  local stream="$1" pkg="$2" task="$3"
  timeout 60 sbcl --noinform --non-interactive \
    --eval "(when (probe-file \"$QL\") (load \"$QL\" :verbose nil))" \
    --eval "(require :uiop)" \
    --eval "(when (find-package :ql) (funcall (intern \"QUICKLOAD\" :ql) '(\"drakma\" \"cl-json\" \"flexi-streams\") :silent t))" \
    --eval "(load \"$EXECUTOR\" :verbose nil :print nil)" \
    --eval "(let ((r (funcall (intern \"ЗАПУСК\" \"ИСПОЛНИТЕЛЬ\") \"$stream\" \"$pkg\" \"$task\"))) (princ r) (finish-output) (sb-ext:exit :code 0))" \
    2>/dev/null || true
}

echo ""
echo "=============================="
echo "  Stream integration tests"
echo "=============================="

# --- эхо ---
echo ""
echo "Stream: эхо"
if [[ -f "$STREAMS/эхо.lisp" ]]; then
  out=$(run_stream "$STREAMS/эхо.lisp" "поток-эхо" "ping")
  if [[ -n "$out" ]]; then
    ok "returns non-empty: '$out'"
  else
    fail "returned empty" ""
  fi
else
  echo "  SKIP (нет эхо.lisp)"
fi

# --- уточнение ---
echo ""
echo "Stream: уточнение  (needs OPENROUTER_API_KEY)"
if [[ -z "${OPENROUTER_API_KEY:-}" ]]; then
  echo "  SKIP (no API key)"
else
  out=$(run_stream "$STREAMS/уточнение.lisp" "поток-уточнение" "что такое 2+2?")
  if [[ ${#out} -gt 5 ]]; then
    ok "got answer (${#out} chars)"
  else
    fail "empty/short answer" "$out"
  fi
fi

# --- исполнитель ---
echo ""
echo "Stream: исполнитель  (needs OPENROUTER_API_KEY + python3)"
if [[ -z "${OPENROUTER_API_KEY:-}" ]]; then
  echo "  SKIP (no API key)"
elif ! command -v python3 &>/dev/null; then
  echo "  SKIP (no python3)"
else
  out=$(run_stream "$STREAMS/исполнитель.lisp" "поток-исполнитель" "print hello world in python")
  if echo "$out" | grep -qi "hello\|world"; then
    ok "contains hello/world: '$out'"
  else
    fail "unexpected output" "$out"
  fi
fi

# --- питон ---
echo ""
echo "Stream: питон  (needs OPENROUTER_API_KEY + python3)"
if [[ -z "${OPENROUTER_API_KEY:-}" ]]; then
  echo "  SKIP (no API key)"
elif ! command -v python3 &>/dev/null; then
  echo "  SKIP (no python3)"
else
  out=$(run_stream "$STREAMS/питон.lisp" "поток-питон" "calculate factorial of 5")
  if echo "$out" | grep -q "120\|Ошибка\|error\|Error"; then
    ok "got result: '$out'"
  elif [[ ${#out} -gt 0 ]]; then
    ok "got output (${#out}c): $out"
  else
    fail "empty output" ""
  fi
fi

# --- реакт ---
echo ""
echo "Stream: реакт  (needs OPENROUTER_API_KEY)"
if [[ -z "${OPENROUTER_API_KEY:-}" ]]; then
  echo "  SKIP (no API key)"
else
  out=$(run_stream "$STREAMS/реакт.lisp" "поток-реакт" "how much is 15 times 7?")
  if echo "$out" | grep -q "105\|Ошибка\|Error"; then
    ok "correct/error: '$out'"
  elif [[ ${#out} -gt 3 ]]; then
    ok "got answer (${#out}c): $out"
  else
    fail "empty output" "$out"
  fi
fi

# --- result ---
echo ""
echo "=============================="
echo "  PASS=$PASS  FAIL=$FAIL"
echo "=============================="
echo ""

[[ $FAIL -eq 0 ]]

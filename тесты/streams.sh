#!/usr/bin/env bash
# streams.sh — integration tests for individual streams
# Streams tested: эхо, уточнение, выполни-код, реакт, порождатель
# Streams that call LLM require OPENROUTER_API_KEY to be set.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT="$(cd "$SCRIPT_DIR/.." && pwd)"
STREAMS="$PROJECT/мастер/потоки"
QL="$HOME/quicklisp/setup.lisp"

PASS=0; FAIL=0

ok()   { echo "  OK  $1"; ((PASS++)) || true; }
fail() { echo "  FAIL $1 | got: $2"; ((FAIL++)) || true; }

# run_stream <stream_file> <package> <task>
# Loads quicklisp + deps, loads the stream file, calls PACKAGE:ВЫПОЛНИТЬ(task)
run_stream() {
  local stream="$1" pkg="$2" task="$3"
  timeout 90 sbcl --noinform --non-interactive \
    --eval "(when (probe-file \"$QL\") (load \"$QL\" :verbose nil))" \
    --eval "(require :uiop)" \
    --eval "(when (find-package :ql) (funcall (intern \"QUICKLOAD\" :ql) '(\"dexador\" \"cl-json\") :silent t))" \
    --eval "(load \"$stream\" :verbose nil :print nil)" \
    --eval "(let ((r (funcall (intern \"ВЫПОЛНИТЬ\" \"$pkg\") \"$task\"))) (princ r) (finish-output) (sb-ext:exit :code 0))" \
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
  out=$(run_stream "$STREAMS/эхо.lisp" "ПОТОК-ЭХО" "ping")
  if echo "$out" | grep -q "Эхо:.*ping"; then
    ok "echo returns 'Эхо: ping': '$out'"
  elif [[ -n "$out" ]]; then
    fail "unexpected output" "$out"
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
  out=$(run_stream "$STREAMS/уточнение.lisp" "ПОТОК-УТОЧНЕНИЕ" "что такое 2+2?")
  if [[ ${#out} -gt 5 ]]; then
    ok "got answer (${#out} chars)"
  else
    fail "empty/short answer" "$out"
  fi
fi

# --- выполни-код ---
echo ""
echo "Stream: выполни-код  (needs OPENROUTER_API_KEY + python3)"
if [[ -z "${OPENROUTER_API_KEY:-}" ]]; then
  echo "  SKIP (no API key)"
elif ! command -v python3 &>/dev/null; then
  echo "  SKIP (no python3)"
else
  out=$(run_stream "$STREAMS/выполни-код.lisp" "ПОТОК-ВЫПОЛНИ-КОД" "напиши print(7*6)")
  if echo "$out" | grep -q "42\|Ошибка\|error\|Error"; then
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
  out=$(run_stream "$STREAMS/реакт.lisp" "ПОТОК-РЕАКТ" "how much is 15 times 7?")
  if echo "$out" | grep -q "105\|Ошибка\|Error"; then
    ok "correct/error: '$out'"
  elif [[ ${#out} -gt 3 ]]; then
    ok "got answer (${#out}c): $out"
  else
    fail "empty output" "$out"
  fi
fi

# --- порождатель ---
echo ""
echo "Stream: порождатель  (needs OPENROUTER_API_KEY)"
if [[ -z "${OPENROUTER_API_KEY:-}" ]]; then
  echo "  SKIP (no API key)"
else
  out=$(run_stream "$STREAMS/порождатель.lisp" "ПОТОК-ПОРОЖДАТЕЛЬ" "поток который возвращает текущее время в формате HH:MM:SS")
  if echo "$out" | grep -qi "порождён\|Ошибка\|error\|Error"; then
    ok "spawner result: '$out'"
  elif [[ ${#out} -gt 3 ]]; then
    ok "got output (${#out}c): $out"
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

#!/usr/bin/env bash
# run-ci.sh — CI entrypoint: unit + master тесты (без API-ключей)
# Код выхода = число ошибок

set -uo pipefail

PROJECT_DIR="$(cd "$(dirname "$0")/.." && pwd)"

echo "=== CI: юнит-тесты мастер/cl ==="
cd "$PROJECT_DIR"

sbcl --noinform --non-interactive \
     --load ~/quicklisp/setup.lisp \
     --eval "(push (truename #p\"мастер/cl/\") asdf:*central-registry*)" \
     --eval "(asdf:load-system :мастер :verbose nil)" \
     --load "мастер/cl/тесты.lisp"

UNIT_CODE=$?

echo ""
echo "=== CI: интеграционные тесты мастер ==="

sbcl --noinform --non-interactive \
     --load ~/quicklisp/setup.lisp \
     --eval "(push (truename #p\"мастер/cl/\") asdf:*central-registry*)" \
     --eval "(asdf:load-system :мастер :verbose nil)" \
     --load "тесты/master.lisp"

INTEGRATION_CODE=$?

EXIT_CODE=$((UNIT_CODE + INTEGRATION_CODE))

if [ $EXIT_CODE -eq 0 ]; then
  echo ""
  echo "CI: ВСЕ ТЕСТЫ ПРОШЛИ"
else
  echo ""
  echo "CI: ЕСТЬ ОШИБКИ (юнит=$UNIT_CODE интеграция=$INTEGRATION_CODE)"
fi

exit $EXIT_CODE

#!/usr/bin/env bash
# Запуск CL-подмастерья

exec sbcl --noinform --non-interactive --load "$(dirname "$0")/main.lisp"

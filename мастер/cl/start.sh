#!/usr/bin/env bash

# Fail on errors
set -e

# Ensure env variables are present for required config
: "${MASTER_BOT_TOKEN:?Needs MASTER_BOT_TOKEN}";
: "${ADMIN_CHAT_ID:?Needs ADMIN_CHAT_ID}";
: "${OPENROUTER_API_KEY:?Needs OPENROUTER_API_KEY}";

# Go to script directory (мастер/cl)
cd "$(dirname "$0")"

# Run sbcl, load quicklisp if present, load ASDF system :мастер, call мастер:start
exec sbcl --noinform \
  --eval '(progn 
     (when (probe-file "../../quicklisp/setup.lisp") (load "../../quicklisp/setup.lisp"))
     (ql:quickload :мастер)
     (in-package :мастер)
     (start))'
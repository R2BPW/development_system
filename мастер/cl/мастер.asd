;;;; мастер.asd — ASDF system definition for :мастер
(asdf:defsystem #:мастер
  :description "Главная система Мастер (Common Lisp Telegram bot/Orchestrator)"
  :author "development_system"
  :license "MIT"
  :depends-on (#:dexador #:cl-json #:hunchentoot #:bordeaux-threads)
  :serial t
  :components ((:file "packages")
               (:file "config")
               (:file "telegram")
               (:file "llm")
               (:file "dusha")
               (:file "история")
               (:file "потоки")
               (:file "комбо")
               (:file "команды")
               (:file "http")
               (:file "main")))

;; Dependencies: dexador cl-json hunchentoot bordeaux-threads (add via Quicklisp) 
;; Place this file in мастер/cl/мастер.asd

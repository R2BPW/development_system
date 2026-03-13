;;; -*- Mode: Lisp; Coding: utf-8 -*-
;;; поток-выполни-код.lisp — тонкая обёртка над поток-питон с моделью gpt-4.1

;;; Гарантируем загрузку питон.lisp (общая реализация)
(let ((питон (merge-pathnames "питон.lisp" (or *load-pathname* *default-pathname-defaults*))))
  (unless (find-package :поток-питон)
    (load питон :verbose nil :print nil)))

(defpackage :поток-выполни-код
  (:use :cl)
  (:export #:выполнить))

(in-package :поток-выполни-код)

(defvar *модель* "openai/gpt-4.1")

(defun выполнить (задача)
  "Делегирует в поток-питон с моделью gpt-4.1."
  (поток-питон:выполнить-с-моделью "выполни-код" задача *модель*))

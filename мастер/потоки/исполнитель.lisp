;;; -*- Mode: Lisp; Coding: utf-8 -*-
;;; поток-исполнитель.lisp — тонкая обёртка над поток-питон с другой моделью

;;; Гарантируем загрузку питон.lisp (общая реализация)
(let ((питон (merge-pathnames "питон.lisp" (or *load-pathname* *default-pathname-defaults*))))
  (unless (find-package :поток-питон)
    (load питон :verbose nil :print nil)))

(defpackage :поток-исполнитель
  (:use :cl)
  (:export #:выполнить))

(in-package :поток-исполнитель)

(defvar *модель* "openai/gpt-4o-mini")

(defun выполнить (задача)
  "Делегирует в поток-питон с моделью gpt-4o-mini."
  (поток-питон:выполнить-с-моделью "исполнитель" задача *модель*))

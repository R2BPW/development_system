;;; -*- Mode: Lisp; Coding: utf-8 -*-
;;; Имя: публикация
;;; Описание: Поток-артефакт — принимает путь к репо и опциональные credentials,
;;;           выполняет git add/commit/push. Внешний эффект: публикация на GitHub.

(defpackage :поток-публикация
  (:use :cl)
  (:export #:выполнить))

(in-package :поток-публикация)

(defun %выполнить-команду (команда каталог)
  "Запустить shell-команду в каталоге. Возвращает (values вывод код)."
  (handler-case
      (multiple-value-bind (вывод ошибки код)
          (uiop:run-program команда
                            :directory каталог
                            :output :string :error-output :string
                            :ignore-error-status t)
        (declare (ignore ошибки))
        (values (string-trim '(#\Space #\Newline) вывод) код))
    (error (e) (values (format nil "Ошибка: ~a" e) 1))))

(defun %git (подкоманда каталог)
  "Обёртка над git. Возвращает (values вывод код)."
  (%выполнить-команду (format nil "git ~a" подкоманда) каталог))

(defun %разобрать-задачу (задача)
  "Извлечь путь и сообщение коммита из задачи.
   Формат: <путь> [сообщение коммита]
   Без сообщения — автоматическое."
  (let* ((части (uiop:split-string задача :separator '(#\Space)))
         (путь (first части))
         (сообщение (if (rest части)
                        (format nil "~{~a~^ ~}" (rest части))
                        (format nil "обновление ~a"
                                (multiple-value-bind (s mi h d mo y) (get-decoded-time)
                                  (declare (ignore s mi h))
                                  (format nil "~4,'0d-~2,'0d-~2,'0d" y mo d))))))
    (values путь сообщение)))

(defun %проверить-репо (путь)
  "Проверить что путь — git-репо."
  (and (probe-file путь)
       (probe-file (merge-pathnames ".git/" путь))))

(defun %статус (каталог)
  "git status --porcelain → строка."
  (%git "status --porcelain" каталог))

(defun %добавить-всё (каталог)
  (%git "add -A" каталог))

(defun %коммит (каталог сообщение)
  (%git (format nil "commit -m ~s" сообщение) каталог))

(defun %пуш (каталог)
  (%git "push" каталог))

(defun выполнить (задача)
  "Точка входа. git add + commit + push.
   Задача: путь-к-репо [сообщение коммита]"
  (handler-case
      (multiple-value-bind (путь сообщение) (%разобрать-задачу задача)
        (unless (%проверить-репо путь)
          (return-from выполнить (format nil "Ошибка: ~a — не git-репо." путь)))
        (let ((статус (%статус путь)))
          (when (zerop (length статус))
            (return-from выполнить "Нет изменений для публикации."))
          (%добавить-всё путь)
          (multiple-value-bind (_ код-коммит) (%коммит путь сообщение)
            (declare (ignore _))
            (when (plusp код-коммит)
              (return-from выполнить "Ошибка при коммите.")))
          (multiple-value-bind (вывод код-пуш) (%пуш путь)
            (if (zerop код-пуш)
                (format nil "✅ Опубликовано: ~a~%~a" сообщение вывод)
                (format nil "⚠️ Коммит создан, но push не удался: ~a" вывод)))))
    (error (e) (format nil "Ошибка: ~a" e))))

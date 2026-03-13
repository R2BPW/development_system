(defpackage :поток-счетчик
  (:use :cl)
  (:export #:выполнить))

(in-package :поток-счетчик)

(defun факториал (n)
  (if (<= n 1)
      1
      (* n (факториал (- n 1)))))

(defun выполнить (задача)
  (handler-case
      (let ((n (parse-integer задача :junk-allowed t)))
        (write-to-string (факториал n)))
    (error () "Ошибка: введите целое неотрицательное число")))
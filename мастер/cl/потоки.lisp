(in-package #:мастер)

(defparameter *активные-потоки* (make-hash-table :test #'equal))

(defun список-потоков ()
  (mapcar #'pathname-name (directory (merge-pathnames "*.lisp" *каталог-потоков*))))

(defun загрузить-поток (путь)
  (let ((ok (not (null (ignore-errors (load путь :verbose nil :print nil))))))
    (format t "[поток] ~A ~A~%" (pathname-name путь) (if ok "OK" "FAIL"))
    ok))

(defun загрузить-все-потоки ()
  (mapc #'загрузить-поток (directory (merge-pathnames "*.lisp" *каталог-потоков*))))

(defun %pkg-name (имя)
  "поток-эхо → ПОТОК-ЭХО"
  (string-upcase (concatenate 'string "поток-" имя)))

(defun %найти-выполнить (имя)
  (let* ((pkg (find-package (%pkg-name имя)))
         (sym (when pkg (find-symbol "ВЫПОЛНИТЬ" pkg))))
    (when (and sym (fboundp sym)) (symbol-function sym))))

(defun запустить-поток (имя задача)
  "Загружает (один раз) и вызывает поток ИМЯ с ЗАДАЧЕЙ."
  (let ((путь (merge-pathnames (format nil "~A.lisp" имя) *каталог-потоков*)))
    (when (probe-file путь)
      (unless (find-package (%pkg-name имя)) (загрузить-поток путь))
      (let ((fn (%найти-выполнить имя)))
        (when fn
          (setf (gethash имя *активные-потоки*) t)
          (funcall fn задача))))))

(defun активные-потоки ()
  (loop for k being the hash-keys of *активные-потоки* collect k))

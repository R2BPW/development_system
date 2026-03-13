(in-package #:мастер)

(defparameter *активные-потоки* (make-hash-table :test #'equal))

(defun список-потоков ()
  "Имена потоков (строки, без .lisp) из каталога потоков."
  (mapcar (lambda (p) (pathname-name p))
          (directory (merge-pathnames "*.lisp" *каталог-потоков*))))

(defun загрузить-поток (путь)
  "Загружает .lisp-файл потока. Возвращает t/nil."
  (not (null (ignore-errors (load путь)))))

(defun загрузить-все-потоки ()
  (dolist (p (directory (merge-pathnames "*.lisp" *каталог-потоков*)))
    (загрузить-поток p)))

(defun %найти-выполнить (имя)
  "Ищет функцию ВЫПОЛНИТЬ в пакете потока."
  (let ((pkg (find-package (string-upcase имя))))
    (when pkg
      (let ((sym (find-symbol "ВЫПОЛНИТЬ" pkg)))
        (when (and sym (fboundp sym)) (symbol-function sym))))))

(defun запустить-поток (имя задача)
  "Запустить поток ИМЯ с задачей ЗАДАЧА. Возвращает строку-результат."
  (let* ((путь (merge-pathnames (format nil "~A.lisp" имя) *каталог-потоков*)))
    (when (probe-file путь)
      (загрузить-поток путь)
      (let ((fn (%найти-выполнить имя)))
        (when fn
          (setf (gethash имя *активные-потоки*) t)
          (funcall fn задача))))))

(defun активные-потоки ()
  (loop for k being the hash-keys of *активные-потоки* collect k))

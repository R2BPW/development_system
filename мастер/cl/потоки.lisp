(in-package #:мастер)

(defparameter *активные-потоки* (make-hash-table :test #'equal))

(defun список-потоков ()
  "Возвращает список имён потоков (без расширения .lisp) из каталога потоков."
  (let* ((files (directory (merge-pathnames "*.lisp" *каталог-потоков*))))
    (mapcar (lambda (path)
              (string-left-trim
               '(#\/) (subseq (namestring path) 0 (- (length (namestring path)) 5))))
            files)))

(defun загрузить-поток (путь)
  "Загружает .lisp-файл потока по абс. пути. true если успех."
  (ignore-errors (load путь)))

(defun загрузить-все-потоки ()
  "Загружает все потоки из каталога потоков."
  (mapc #'загрузить-поток
        (directory (merge-pathnames "*.lisp" *каталог-потоков*))))

(defun запустить-поток (имя задача)
  "Запустить поток с данным ИМЯ и задачей (строка). Возвращает вывод."
  (let* ((путь (merge-pathnames (format nil "~A.lisp" имя) *каталог-потоков*)))
    (when (probe-file путь)
      (загрузить-поток путь)
      (let ((fn (or (ignore-errors (symbol-function 'выполнить))
                    (fdefinition (find-symbol "ВЫПОЛНИТЬ" :cl-user)))))
        (when fn
          (setf (gethash имя *активные-потоки*) t)
          (funcall fn задача))))))

(defun активные-потоки ()
  "Список активных потоков (имён)"
  (loop for k being the hash-keys of *активные-потоки* collect k))

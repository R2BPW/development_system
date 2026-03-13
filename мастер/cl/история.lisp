;;; история.lisp — Хранение истории диалогов (json) per chat-id
(in-package #:мастер)

(defparameter *json-lib* :cl-json)

(defun путь-истории (chat-id)
  (merge-pathnames (format nil "история/~A.json" chat-id) *каталог-истории*))

(defun загрузить-историю (chat-id)
  (let ((файл (путь-истории chat-id)))
    (if (probe-file файл)
        (with-open-file (in файл :direction :input)
          (let ((data (read-line in nil)))
            (if data
                (cl-json:decode-json-from-string data)
                nil)))
        nil)))

(defun сохранить-историю (chat-id история)
  (let ((файл (путь-истории chat-id)))
    (ensure-directories-exist файл)
    (with-open-file (out файл :direction :output :if-exists :supersede :if-does-not-exist :create)
      (write-string (cl-json:encode-json-to-string история) out))))

(defun добавить-сообщение (chat-id role content)
  (let* ((история (or (загрузить-историю chat-id) '()))
         (msg (list :role role :content content))
         (новая-история (append история (list msg))))
    (сохранить-историю chat-id новая-история)
    новая-история))

(defun очистить-историю (chat-id)
  (let ((файл (путь-истории chat-id)))
    (when (probe-file файл)
      (delete-file файл))))

(in-package #:мастер)

(defun %путь-истории (chat-id)
  (merge-pathnames (format nil "~A.json" chat-id) *каталог-истории*))

(defun загрузить-историю (chat-id)
  (let ((файл (%путь-истории chat-id)))
    (when (probe-file файл)
      (handler-case
          (cl-json:decode-json-from-string (uiop:read-file-string файл))
        (error () nil)))))

(defun сохранить-историю (chat-id история)
  (let ((файл (%путь-истории chat-id)))
    (ensure-directories-exist файл)
    (uiop:with-output-file (out файл :if-exists :supersede)
      (write-string (cl-json:encode-json-to-string история) out))))

(defun добавить-сообщение (chat-id role content)
  (let* ((история (or (загрузить-историю chat-id) '()))
         (новая (append история (list `((:role . ,role) (:content . ,content))))))
    (сохранить-историю chat-id новая)
    новая))

(defun очистить-историю (chat-id)
  (let ((файл (%путь-истории chat-id)))
    (when (probe-file файл) (delete-file файл))))

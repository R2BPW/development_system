(in-package #:мастер)

(defparameter *токен* (uiop:getenv "MASTER_BOT_TOKEN"))
(defparameter *admin-chat-id*
  (let ((s (uiop:getenv "ADMIN_CHAT_ID")))
    (when (and s (plusp (length s))) (parse-integer s))))

;; Корень = мастер/ (родитель мастер/cl/ где лежит .asd)
(defparameter *корень*
  (uiop:pathname-parent-directory-pathname
   (asdf:system-source-directory :мастер)))

(defparameter *путь-души*
  (merge-pathnames #P"душа.scm" *корень*))

(defparameter *каталог-потоков*
  (merge-pathnames #P"потоки/" *корень*))

(defparameter *каталог-истории*
  (merge-pathnames #P"история/" *корень*))

;;; ─── логирование ────────────────────────────────────────────────────────────

(defun %timestamp ()
  (multiple-value-bind (s mi h d mo y) (get-decoded-time)
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D" y mo d h mi s)))

(defun log/info (tag fmt &rest args)
  "Информационное сообщение: [timestamp] [tag] message"
  (format t "~A [~A] ~?~%" (%timestamp) tag fmt args))

(defun log/error (tag fmt &rest args)
  "Сообщение об ошибке: [timestamp] [tag] message → *error-output*"
  (format *error-output* "~A [~A] ~?~%" (%timestamp) tag fmt args))

(in-package #:мастер)

(defparameter *токен* (uiop:getenv "MASTER_BOT_TOKEN"))
(defparameter *admin-id* (uiop:getenv "ADMIN_CHAT_ID"))

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

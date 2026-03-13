(in-package #:мастер)

(defparameter *токен* (uiop:getenv "MASTER_BOT_TOKEN"))
(defparameter *admin-id* (uiop:getenv "ADMIN_CHAT_ID"))

(defparameter *корень*
  (uiop:pathname-parent-directory-pathname (uiop:getcwd)))

(defparameter *путь-души*
  (merge-pathnames #P"душа.scm" *корень*))

(defparameter *каталог-потоков*
  (merge-pathnames #P"потоки/" *корень*))

(defparameter *каталог-истории*
  (merge-pathnames #P"история/" *корень*))

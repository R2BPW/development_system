(in-package #:мастер)

(defparameter *токен*
  (or (uiop:getenv "MASTER_BOT_TOKEN")
      (error "Переменная среды MASTER_BOT_TOKEN не установлена")))

(defparameter *admin-id*
  (let ((id (uiop:getenv "ADMIN_CHAT_ID")))
    (or id (error "Переменная среды ADMIN_CHAT_ID не установлена"))))

(defparameter *корень*
  (uiop:pathname-parent-directory-pathname (uiop:getcwd)))

(defparameter *путь-души*
  (merge-pathnames #P"душа.scm" *корень*))

(defparameter *каталог-потоков*
  (merge-pathnames #P"потоки/" *корень*))

(defparameter *каталог-истории*
  (merge-pathnames #P"история/" *корень*))

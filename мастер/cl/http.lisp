;;; http.lisp — HTTP API server для мастера
(in-package #:мастер)

(defparameter *http-acceptor* nil
  "Hunchentoot acceptor для HTTP API")

(defparameter *web-token* nil
  "Токен авторизации для Web API")

(defun %generate-token ()
  "Генерирует случайный токен"
  (format nil "~36R" (random (expt 2 128))))

(defun %init-web-token ()
  "Инициализирует токен из env или генерирует новый"
  (let ((token (uiop:getenv "WEB_API_TOKEN")))
    (if token
        (setf *web-token* token)
        (progn
          (setf *web-token* (%generate-token))
          (log/info "Generated WEB_API_TOKEN: ~A" *web-token*)))))

(defun %get-port ()
  "Возвращает порт из env WEB_PORT или 7070"
  (let ((port-str (uiop:getenv "WEB_PORT")))
    (if port-str
        (parse-integer port-str)
        7070)))

(defun start-http-server ()
  "Создаёт и запускает HTTP acceptor"
  (when (null *web-token*)
    (%init-web-token))
  (setf *http-acceptor*
        (make-instance 'hunchentoot:easy-acceptor
                       :port (%get-port)))
  (hunchentoot:start *http-acceptor*)
  (log/info "HTTP server started on port ~A" (%get-port)))

(defun stop-http-server ()
  "Останавливает HTTP acceptor"
  (when *http-acceptor*
    (hunchentoot:stop *http-acceptor*)
    (setf *http-acceptor* nil)
    (log/info "HTTP server stopped")))

(defun %check-auth (request)
  "Проверяет заголовок Authorization Bearer, возвращает t/nil"
  (let* ((auth-header (hunchentoot:header-in :authorization request))
         (expected (format nil "Bearer ~A" *web-token*)))
    (and auth-header (string= auth-header expected))))

(defun %json-ok (data)
  "Сериализует data через cl-json, ставит Content-Type, возвращает строку"
  (setf (hunchentoot:content-type*) "application/json")
  (cl-json:encode-json-to-string data))

(defun %json-error (msg status)
  "Возвращает JSON ошибку с заданным статусом"
  (setf (hunchentoot:return-code*) status)
  (setf (hunchentoot:content-type*) "application/json")
  (cl-json:encode-json-to-string `((:error . ,msg))))

;; ─── HTTP Routes ─────────────────────────────────────────────────────────────

(hunchentoot:define-easy-handler (api-flows :uri "/api/flows") ()
  (if (not (%check-auth hunchentoot:*request*))
      (%json-error "Unauthorized" 401)
      (%json-ok `((:flows . ,(активные-потоки))))))

(hunchentoot:define-easy-handler (api-dialog :uri "/api/dialog"
                                              :default-request-type :post) ()
  (if (not (%check-auth hunchentoot:*request*))
      (%json-error "Unauthorized" 401)
      (let* ((body (hunchentoot:raw-post-data :force-text t))
             (json (cl-json:decode-json-from-string body))
             (text (cdr (assoc :text json))))
        (if (null text)
            (%json-error "Missing 'text' field" 400)
            (handler-case
                (let ((response (обработать-команду 0 text)))
                  (%json-ok `((:response . ,response))))
              (error (e)
                (%json-error (format nil "~A" e) 500)))))))

(hunchentoot:define-easy-handler (api-run-flow :uri "/api/flows/run"
                                                :default-request-type :post) ()
  (if (not (%check-auth hunchentoot:*request*))
      (%json-error "Unauthorized" 401)
      (let* ((body (hunchentoot:raw-post-data :force-text t))
             (json (cl-json:decode-json-from-string body))
             (flow (cdr (assoc :flow json)))
             (task (cdr (assoc :task json))))
        (cond
          ((null flow) (%json-error "Missing 'flow' field" 400))
          ((null task) (%json-error "Missing 'task' field" 400))
          (t (handler-case
                 (let ((result (запустить-поток flow task)))
                   (if result
                       (%json-ok `((:result . ,result)))
                       (%json-error (format nil "Flow '~A' not found" flow) 404)))
               (error (e)
                 (%json-error (format nil "~A" e) 500))))))))

(hunchentoot:define-easy-handler (api-spawn-flow :uri "/api/flows/spawn"
                                                  :default-request-type :post) ()
  (if (not (%check-auth hunchentoot:*request*))
      (%json-error "Unauthorized" 401)
      (let* ((body (hunchentoot:raw-post-data :force-text t))
             (json (cl-json:decode-json-from-string body))
             (description (cdr (assoc :description json))))
        (if (null description)
            (%json-error "Missing 'description' field" 400)
            (handler-case
                (let* ((command (concatenate 'string "/породить " description))
                       (result (обработать-команду 0 command)))
                  (%json-ok `((:result . ,result))))
              (error (e)
                (%json-error (format nil "~A" e) 500)))))))

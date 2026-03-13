;;;; master.lisp — интеграционные тесты мастер/cl
;;;; Покрывает сценарии из таблицы edge cases + полный pipeline

(in-package #:мастер)

(defvar *интеграция-ошибки* 0)

(defmacro итест (имя &body тело)
  `(handler-case
       (progn ,@тело (format t "  ✓ ~A~%" ,имя))
     (error (e)
       (incf *интеграция-ошибки*)
       (format t "  ✗ ~A: ~A~%" ,имя e))))

;; с-моками может быть не определён если тесты.lisp не загружен
(unless (macro-function 'с-моками)
  (defmacro с-моками ((&key api-post llm) &body body)
    "Подменяет *api-post-fn* и/или *llm-fn* на время выполнения body."
    `(let (,@(when api-post `((*api-post-fn* ,api-post)))
           ,@(when llm `((*llm-fn* ,llm))))
       ,@body)))

(format t "~%[ИНТЕГРАЦИОННЫЕ ТЕСТЫ МАСТЕР]~%")

;; ── Edge case 1: Offset persistence ────────────────────────────────────────

(итест "offset persistence — save/load roundtrip"
  (let ((*offset-file* "/tmp/test-offset-integration.txt"))
    (when (probe-file *offset-file*) (delete-file *offset-file*))
    (%save-offset 42)
    (assert (= (%load-offset) 42))
    (delete-file *offset-file*)))

(итест "offset persistence — load без файла → 0"
  (let ((*offset-file* "/tmp/test-offset-nonexistent-9999.txt"))
    (when (probe-file *offset-file*) (delete-file *offset-file*))
    (assert (= (%load-offset) 0))))

(итест "offset persistence — перезапись обновляет"
  (let ((*offset-file* "/tmp/test-offset-overwrite.txt"))
    (when (probe-file *offset-file*) (delete-file *offset-file*))
    (%save-offset 10)
    (%save-offset 20)
    (assert (= (%load-offset) 20))
    (delete-file *offset-file*)))

;; ── Edge case 2: Rate limit 429 — api-post → nil → poll продолжается ─────

(итест "rate limit — api-post nil не ломает обработку"
  (let ((обработано nil))
    (с-моками (:api-post (lambda (method payload)
                            (declare (ignore method payload))
                            nil)  ; имитируем 429/ошибку
               :llm (lambda (prompt &key model system messages)
                      (declare (ignore prompt model system messages))
                      "ответ"))
      (let ((*admin-chat-id* 100))
        ;; dispatch при api-post=nil не должен бросать ошибку
        (handler-case
            (progn (%dispatch 100 "/потоки") (setf обработано t))
          (error () (setf обработано nil)))
        (assert обработано)))))

(итест "rate limit — send-message с nil api-post не падает"
  (с-моками (:api-post (lambda (m p) (declare (ignore m p)) nil))
    (handler-case
        (progn (send-message 123 "тест") t)
      (error () (assert nil)))))

(итест "rate limit — обработать-update при api-post=nil"
  (с-моками (:api-post (lambda (m p) (declare (ignore m p)) nil)
             :llm (lambda (prompt &key model system messages)
                    (declare (ignore prompt model system messages))
                    "мок"))
    (let ((*admin-chat-id* 55))
      (handler-case
          (progn
            (обработать-update
             '((:update--id . 1)
               (:message . ((:text . "/потоки")
                            (:chat . ((:id . 55)))))))
            t)
        (error () (assert nil))))))

;; ── Edge case 3: Broken stream — синтаксическая ошибка → FAIL ────────────

(итест "broken stream — load возвращает nil, лог FAIL"
  (let ((tmp "/tmp/test-broken-stream.lisp"))
    (with-open-file (out tmp :direction :output :if-exists :supersede)
      (write-string "(defpackage :поток-сломанный (:use #:cl))
(in-package :поток-сломанный)
(this-is-broken " out))  ; незакрытая скобка
    (let ((result (загрузить-поток (pathname tmp))))
      (assert (null result)))
    (delete-file tmp)))

(итест "broken stream — остальные потоки грузятся"
  ;; после загрузки сломанного потока, эхо должен быть доступен
  (clrhash *активные-потоки*)
  (загрузить-все-потоки)
  (assert (plusp (hash-table-count *активные-потоки*)))
  ;; эхо-поток точно загружен
  (assert (not (null (find-package "ПОТОК-ЭХО")))))

;; ── Edge case 4: Nil text guard — send-message(id, nil) → ничего ─────────

(итест "nil text guard — send-message nil → api-post не вызывается"
  (let ((вызовы 0))
    (с-моками (:api-post (lambda (m p) (declare (ignore m p)) (incf вызовы) nil))
      (send-message 123 nil)
      (assert (= вызовы 0)))))

(итест "nil text guard — send-message пустая строка → api-post не вызывается"
  (let ((вызовы 0))
    (с-моками (:api-post (lambda (m p) (declare (ignore m p)) (incf вызовы) nil))
      (send-message 123 "")
      (assert (= вызовы 0)))))

(итест "nil text guard — send-message нормальный текст → api-post вызывается"
  (let ((вызовы 0))
    (с-моками (:api-post (lambda (m p) (declare (ignore m p)) (incf вызовы) '((:ok . t))))
      (send-message 123 "привет")
      (assert (= вызовы 1)))))

;; ── Интеграция: обработать-update — полный pipeline ──────────────────────

(итест "обработать-update message → dispatch → send-message"
  (let ((отправлено nil))
    (с-моками (:api-post (lambda (method payload)
                            (when (string= method "sendMessage")
                              (setf отправлено payload))
                            '((:ok . t))))
      (let ((*admin-chat-id* 77))
        (обработать-update
         '((:update--id . 100)
           (:message . ((:text . "/потоки")
                        (:chat . ((:id . 77)))))))
        (assert (not (null отправлено)))
        (let ((текст (cdr (assoc :text отправлено))))
          (assert (search "Потоки:" текст)))))))

(итест "обработать-update callback-query → dispatch"
  (let ((вызовы '()))
    (с-моками (:api-post (lambda (method payload)
                            (declare (ignore payload))
                            (push method вызовы)
                            '((:ok . t))))
      (let ((*admin-chat-id* 88))
        (обработать-update
         `((:update--id . 200)
           (:callback--query
            . ((:id . "cb-1")
               (:data . "/потоки")
               (:message . ((:chat . ((:id . 88)))))))))
        ;; answerCallbackQuery + sendMessage
        (assert (member "answerCallbackQuery" вызовы :test #'string=))
        (assert (member "sendMessage" вызовы :test #'string=))))))

(итест "обработать-update message без text → ничего не происходит"
  (let ((вызовы 0))
    (с-моками (:api-post (lambda (m p) (declare (ignore m p)) (incf вызовы) nil))
      (обработать-update
       '((:update--id . 300)
         (:message . ((:chat . ((:id . 99)))))))
      ;; text отсутствует → dispatch не вызывается → api-post не вызывается
      (assert (= вызовы 0)))))

(итест "обработать-update свободный текст → LLM"
  (let ((отправлено nil))
    (с-моками (:api-post (lambda (method payload)
                            (when (string= method "sendMessage")
                              (setf отправлено payload))
                            '((:ok . t)))
               :llm (lambda (prompt &key model system messages)
                      (declare (ignore model system messages))
                      (format nil "интеграция: ~A" prompt)))
      (let ((*admin-chat-id* 44))
        (очистить-историю 44)
        (обработать-update
         '((:update--id . 400)
           (:message . ((:text . "привет мир")
                        (:chat . ((:id . 44)))))))
        (assert (not (null отправлено)))
        (let ((текст (cdr (assoc :text отправлено))))
          (assert (search "интеграция:" текст)))
        (очистить-историю 44)))))

(итест "обработать-update чужой chat-id → Доступ запрещён"
  (let ((отправлено nil))
    (с-моками (:api-post (lambda (method payload)
                            (when (string= method "sendMessage")
                              (setf отправлено payload))
                            '((:ok . t))))
      (let ((*admin-chat-id* 11))
        (обработать-update
         '((:update--id . 500)
           (:message . ((:text . "/потоки")
                        (:chat . ((:id . 999)))))))
        (assert (not (null отправлено)))
        (let ((текст (cdr (assoc :text отправлено))))
          (assert (search "Доступ запрещён" текст)))))))

;; ── Интеграция: полный цикл команд через pipeline ────────────────────────

(итест "pipeline /старт → текст + markup"
  (let ((отправлено nil))
    (с-моками (:api-post (lambda (method payload)
                            (when (string= method "sendMessage")
                              (setf отправлено payload))
                            '((:ok . t))))
      (let ((*admin-chat-id* 33))
        (обработать-update
         '((:update--id . 600)
           (:message . ((:text . "/старт")
                        (:chat . ((:id . 33)))))))
        (assert (not (null отправлено)))
        ;; старт отправляет reply_markup (главное меню)
        (assert (cdr (assoc :reply--markup отправлено)))))))

(итест "pipeline /запустить эхо ping → результат"
  (let ((отправлено nil))
    (с-моками (:api-post (lambda (method payload)
                            (when (string= method "sendMessage")
                              (setf отправлено payload))
                            '((:ok . t))))
      (let ((*admin-chat-id* 33))
        (обработать-update
         '((:update--id . 700)
           (:message . ((:text . "/запустить эхо ping")
                        (:chat . ((:id . 33)))))))
        (assert (not (null отправлено)))
        (let ((текст (cdr (assoc :text отправлено))))
          (assert (search "Эхо:" текст)))))))

(итест "pipeline /сбросить → очищает историю"
  (let ((*admin-chat-id* 66))
    (добавить-сообщение 66 "user" "тест")
    (assert (not (null (загрузить-историю 66))))
    (с-моками (:api-post (lambda (m p) (declare (ignore m p)) '((:ok . t))))
      (обработать-update
       '((:update--id . 800)
         (:message . ((:text . "/сбросить")
                      (:chat . ((:id . 66)))))))
      (assert (null (загрузить-историю 66))))))

(итест "pipeline /состояние → включает активные"
  (загрузить-все-потоки)
  (let ((отправлено nil))
    (с-моками (:api-post (lambda (method payload)
                            (when (string= method "sendMessage")
                              (setf отправлено payload))
                            '((:ok . t))))
      (let ((*admin-chat-id* 33))
        (обработать-update
         '((:update--id . 900)
           (:message . ((:text . "/состояние")
                        (:chat . ((:id . 33)))))))
        (assert (not (null отправлено)))
        (let ((текст (cdr (assoc :text отправлено))))
          (assert (search "Активные:" текст)))))))

(итест "pipeline /остановить → очищает активные потоки"
  (загрузить-все-потоки)
  (assert (plusp (hash-table-count *активные-потоки*)))
  (let ((отправлено nil))
    (с-моками (:api-post (lambda (method payload)
                            (when (string= method "sendMessage")
                              (setf отправлено payload))
                            '((:ok . t))))
      (let ((*admin-chat-id* 33))
        (обработать-update
         '((:update--id . 1000)
           (:message . ((:text . "/остановить")
                        (:chat . ((:id . 33)))))))
        (assert (= (hash-table-count *активные-потоки*) 0))
        (let ((текст (cdr (assoc :text отправлено))))
          (assert (search "остановлен" текст)))))))

;; ── Результат ────────────────────────────────────────────────────────────

(format t "~%Интеграционные ошибки: ~A~%" *интеграция-ошибки*)
(when (plusp *интеграция-ошибки*) (sb-ext:exit :code 1))

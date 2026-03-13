;;;; тесты.lisp — smoke-тесты мастер/cl
;;;; Запуск: sbcl --load ~/quicklisp/setup.lisp \
;;;;           --eval "(push #p\"мастер/cl/\" asdf:*central-registry*)" \
;;;;           --eval "(asdf:load-system :мастер :verbose nil)" \
;;;;           --load мастер/cl/тесты.lisp --eval "(sb-ext:exit)"

(in-package #:мастер)

(defparameter *тест-ошибки* 0)

(defmacro тест (имя &body тело)
  `(handler-case
       (progn ,@тело (format t "  ✓ ~A~%" ,имя))
     (error (e)
       (incf *тест-ошибки*)
       (format t "  ✗ ~A: ~A~%" ,имя e))))

(defmacro с-моками ((&key api-post llm) &body body)
  "Подменяет *api-post-fn* и/или *llm-fn* на время выполнения body."
  `(let (,@(when api-post `((*api-post-fn* ,api-post)))
         ,@(when llm `((*llm-fn* ,llm))))
     ,@body))

(format t "[ТЕСТЫ МАСТЕР]~%")

;; 1. api-url формат
(тест "api-url формат"
  (let ((url (api-url "getUpdates")))
    (assert (search "telegram.org/bot" url))
    (assert (search "getUpdates" url))))

;; 2. душа->системный-промпт — новый формат (душа (ключ значение) ...)
(тест "душа->системный-промпт"
  (let* ((фикстура '(душа (указание "Тест-бот") (девиз "краткий")))
         (промпт (душа->системный-промпт фикстура)))
    (assert (search "Тест-бот" промпт))
    (assert (search "краткий" промпт))))

;; 3. %душа-значение — извлечение из структуры
(тест "%душа-значение"
  (let ((д '(душа (указание "инструкция") (модель "gpt-4") (теплота 0.5))))
    (assert (string= (%душа-значение д 'указание) "инструкция"))
    (assert (string= (%душа-значение д 'модель) "gpt-4"))
    (assert (null (%душа-значение д 'несуществующий)))))

;; 4. читать-душу — валидация формата
(тест "читать-душу валидация"
  (let ((tmp "/tmp/test-dusha-bad.scm"))
    (with-open-file (out tmp :direction :output :if-exists :supersede)
      (prin1 '(не-душа (a 1)) out))
    (assert (null (ignore-errors (читать-душу tmp))))))

;; 5. список-потоков — возвращает список строк
(тест "список-потоков возвращает строки"
  (let ((lst (список-потоков)))
    (assert (listp lst))
    (dolist (имя lst)
      (assert (stringp имя)))))

;; 6. обработать-команду /старт → строка (не nil)
(тест "обработать-команду /старт → строка"
  (let ((resp (обработать-команду 0 "/старт")))
    (assert (stringp resp))
    (assert (plusp (length resp)))))

;; 7. %обр-старт возвращает (values строка markup)
(тест "%обр-старт возвращает values"
  (multiple-value-bind (текст markup) (%обр-старт 0 nil)
    (assert (stringp текст))
    (assert (plusp (length текст)))
    (assert markup)))

;; 8. %split-words
(тест "%split-words базовый"
  (let ((res (%split-words "/запустить поток задача")))
    (assert (equal res '("/запустить" "поток" "задача")))))

(тест "%split-words пустая строка"
  (assert (null (%split-words ""))))

;; 9. %llm-key ленивое чтение
(тест "%llm-key ленивое"
  (let ((*llm-key* "test-key"))
    (assert (string= (%llm-key) "test-key"))))

;; 10. обработать-команду неизвестная
(тест "обработать-команду неизвестная → строка"
  (let ((resp (обработать-команду 0 "/хрень")))
    (assert (stringp resp))))

;; ── Фаза 2: инъекция зависимостей ──────────────────────────────────────────

;; 11. с-моками подменяет api-post
(тест "с-моками api-post"
  (let ((вызовы '()))
    (с-моками (:api-post (lambda (method payload)
                            (push (list method payload) вызовы)
                            '((:ok . t))))
      (send-message 123 "тест")
      (assert (= 1 (length вызовы)))
      (assert (string= "sendMessage" (first (first вызовы)))))))

;; 12. с-моками подменяет llm
(тест "с-моками llm"
  (с-моками (:llm (lambda (prompt &key model system messages)
                     (declare (ignore prompt model system messages))
                     "мок-ответ"))
    (let ((resp (llm-complete "привет")))
      (assert (string= resp "мок-ответ")))))

;; 13. обработать-команду /потоки с заглушкой IO
(тест "обработать-команду /потоки с моком api-post"
  (с-моками (:api-post (lambda (m p) (declare (ignore m p)) nil))
    (let ((resp (обработать-команду 0 "/потоки")))
      (assert (stringp resp)))))

;; 14. обработать-команду /старт с заглушкой IO
(тест "обработать-команду /старт с моком api-post"
  (с-моками (:api-post (lambda (m p) (declare (ignore m p)) nil))
    (let ((текст (обработать-команду 0 "/старт")))
      (assert (stringp текст))
      (assert (plusp (length текст))))))

;; 15. свободный текст → LLM-mock
(тест "свободный текст → llm-mock"
  (с-моками (:llm (lambda (prompt &key model system messages)
                     (declare (ignore model system messages))
                     (format nil "эхо: ~A" prompt)))
    (let ((resp (обработать-команду 0 "просто текст")))
      (assert (stringp resp))
      (assert (search "эхо:" resp)))))

;; 16. %dispatch с двойным моком — проверяем что send-message вызывается
(тест "%dispatch полный цикл с моками"
  (let ((отправлено nil))
    (с-моками (:api-post (lambda (method payload)
                            (when (string= method "sendMessage")
                              (setf отправлено payload))
                            '((:ok . t)))
               :llm (lambda (prompt &key model system messages)
                      (declare (ignore prompt model system messages))
                      "ответ-мок"))
      (let ((*admin-chat-id* 42))
        (%dispatch 42 "/потоки")
        (assert (not (null отправлено)))))))

;; ── Фаза 3: юнит-тесты (без IO) ─────────────────────────────────────────────

;; 17. %split-words — пустая строка → nil
(тест "%split-words пустая → nil"
  (assert (null (%split-words ""))))

;; 18. %split-words — множественные пробелы
(тест "%split-words множественные пробелы"
  (assert (equal (%split-words "a  b") '("a" "b"))))

;; 19. %split-words — пробелы по краям
(тест "%split-words пробелы по краям"
  (assert (equal (%split-words "  hello  ") '("hello"))))

;; 20. %split-words — один элемент
(тест "%split-words один элемент"
  (assert (equal (%split-words "слово") '("слово"))))

;; 21. %cmd-args — команда с аргументами
(тест "%cmd-args команда с аргументами"
  (multiple-value-bind (cmd args) (%cmd-args "/запустить эхо задача")
    (assert (string= cmd "/запустить"))
    (assert (equal args '("эхо" "задача")))))

;; 22. %cmd-args — пустая строка
(тест "%cmd-args пустая строка"
  (multiple-value-bind (cmd args) (%cmd-args "")
    (assert (string= cmd ""))
    (assert (null args))))

;; 23. %cmd-args — только команда без аргументов
(тест "%cmd-args только команда"
  (multiple-value-bind (cmd args) (%cmd-args "/потоки")
    (assert (string= cmd "/потоки"))
    (assert (null args))))

;; 24. %cmd-args — downcases команду
(тест "%cmd-args downcases"
  (multiple-value-bind (cmd args) (%cmd-args "/ПОТОКИ аргумент")
    (assert (string= cmd "/потоки"))
    (assert (equal args '("аргумент")))))

;; 25. %кнопка->команда — точный матч кнопок
(тест "%кнопка->команда Потоки"
  (assert (string= (%кнопка->команда "Потоки") "/потоки")))

(тест "%кнопка->команда Запустить"
  (assert (string= (%кнопка->команда "Запустить") "/запустить")))

(тест "%кнопка->команда Породить поток"
  (assert (string= (%кнопка->команда "Породить поток") "/породить")))

(тест "%кнопка->команда Состояние"
  (assert (string= (%кнопка->команда "Состояние") "/состояние")))

(тест "%кнопка->команда Диалог"
  (assert (string= (%кнопка->команда "Диалог") "/диалог")))

(тест "%кнопка->команда Сбросить"
  (assert (string= (%кнопка->команда "Сбросить") "/сбросить")))

(тест "%кнопка->команда Остановить"
  (assert (string= (%кнопка->команда "Остановить") "/остановить")))

;; 26. %кнопка->команда — неизвестная кнопка → nil
(тест "%кнопка->команда неизвестная → nil"
  (assert (null (%кнопка->команда "Несуществующая"))))

;; 27. %pkg-name — стандартные случаи
(тест "%pkg-name эхо"
  (assert (string= (%pkg-name "эхо") "ПОТОК-ЭХО")))

(тест "%pkg-name порождатель"
  (assert (string= (%pkg-name "порождатель") "ПОТОК-ПОРОЖДАТЕЛЬ")))

(тест "%pkg-name латиница"
  (assert (string= (%pkg-name "test") "ПОТОК-TEST")))

;; ── Фаза 4: состояние потоков и история ─────────────────────────────────────

;; 28. добавить-сообщение × 25 → длина ≤ 20
(тест "добавить-сообщение ограничение длины"
  (let ((test-id "test-limit-9999"))
    (очистить-историю test-id)
    (dotimes (i 25)
      (добавить-сообщение test-id "user" (format nil "msg-~A" i)))
    (let ((история (загрузить-историю test-id)))
      (assert (<= (length история) *макс-история*))
      ;; последнее сообщение должно быть msg-24
      (let* ((последнее (car (last история)))
             (content (cdr (assoc :content последнее))))
        (assert (string= content "msg-24"))))
    (очистить-историю test-id)))

;; 29. добавить-сообщение сохраняет последние *макс-история* записей
(тест "добавить-сообщение хранит последние записи"
  (let ((test-id "test-last-9999")
        (*макс-история* 5))
    (очистить-историю test-id)
    (dotimes (i 10)
      (добавить-сообщение test-id "user" (format nil "m~A" i)))
    (let ((история (загрузить-историю test-id)))
      (assert (= (length история) 5))
      ;; первый сохранённый = m5
      (let ((first-content (cdr (assoc :content (first история)))))
        (assert (string= first-content "m5"))))
    (очистить-историю test-id)))

;; 30. загрузить-все-потоки заполняет *активные-потоки*
(тест "загрузить-все-потоки → активные-потоки непустые"
  (clrhash *активные-потоки*)
  (загрузить-все-потоки)
  (assert (plusp (hash-table-count *активные-потоки*))))

;; 31. /состояние непустой после загрузки потоков
(тест "/состояние непустой после загрузки"
  ;; потоки уже загружены тестом 30
  (let ((resp (обработать-команду 0 "/состояние")))
    (assert (stringp resp))
    (assert (search "Активные:" resp))))

;; 32. *макс-история* по умолчанию = 20
(тест "*макс-история* по умолчанию 20"
  (assert (= *макс-история* 20)))

;; ── Фаза 5: юзабилити ─────────────────────────────────────────────────────

;; 33. /запустить без аргументов → inline-клавиатура
(тест "/запустить без аргументов → inline-keyboard"
  (multiple-value-bind (текст markup) (%обр-запустить 0 nil)
    (assert (stringp текст))
    (assert (search "Выберите" текст))
    (assert markup)
    (assert (assoc :inline--keyboard markup))))

;; 34. /запустить с одним аргументом → подсказка задачи
(тест "/запустить с одним аргументом → подсказка задачи"
  (let ((resp (%обр-запустить 0 '("эхо"))))
    (assert (stringp resp))
    (assert (search "задач" resp))
    (assert (search "эхо" resp))))

;; 35. %dispatch с чужим chat-id → "Доступ запрещён"
(тест "%dispatch чужой chat-id → Доступ запрещён"
  (let ((отправлено nil))
    (с-моками (:api-post (lambda (method payload)
                            (when (string= method "sendMessage")
                              (setf отправлено payload))
                            '((:ok . t))))
      (let ((*admin-chat-id* 42))
        (%dispatch 999 "/потоки")
        (assert (not (null отправлено)))
        (let ((текст (cdr (assoc :text отправлено))))
          (assert (search "Доступ запрещён" текст)))))))

;; 36. %dispatch с admin chat-id → нормальный ответ (не "Доступ запрещён")
(тест "%dispatch admin chat-id → нормальный ответ"
  (let ((отправлено nil))
    (с-моками (:api-post (lambda (method payload)
                            (when (string= method "sendMessage")
                              (setf отправлено payload))
                            '((:ok . t))))
      (let ((*admin-chat-id* 42))
        (%dispatch 42 "/потоки")
        (assert (not (null отправлено)))
        (let ((текст (cdr (assoc :text отправлено))))
          (assert (null (search "Доступ запрещён" текст))))))))

;; 37. обработать-команду кнопка "Запустить" → inline-keyboard
(тест "кнопка Запустить → inline-keyboard"
  (multiple-value-bind (текст markup) (обработать-команду 0 "Запустить")
    (assert (stringp текст))
    (assert markup)
    (assert (assoc :inline--keyboard markup))))

;; ── Фаза 8: рефакторинг ─────────────────────────────────────────────────────

;; 38. log/info выводит timestamp и тег
(тест "log/info формат"
  (let ((out (with-output-to-string (*standard-output*)
               (log/info "тест" "сообщение ~A" 42))))
    (assert (search "[тест]" out))
    (assert (search "сообщение 42" out))
    ;; timestamp формат YYYY-MM-DD HH:MM:SS
    (assert (>= (length out) 19))))

;; 39. log/error выводит в *error-output*
(тест "log/error формат"
  (let ((out (with-output-to-string (*error-output*)
               (log/error "err" "ошибка ~A" "тест"))))
    (assert (search "[err]" out))
    (assert (search "ошибка тест" out))))

;; 40. %timestamp формат
(тест "%timestamp формат YYYY-MM-DD"
  (let ((ts (мастер::%timestamp)))
    (assert (= (length ts) 19))
    (assert (char= (char ts 4) #\-))
    (assert (char= (char ts 10) #\Space))))

;; 41. ignore-errors заменён — %save-offset не бросает
(тест "%save-offset не бросает при ошибке"
  (let ((*offset-file* "/nonexistent-dir/offset.txt"))
    ;; handler-case ловит ошибку, не бросает наружу
    (let ((err (with-output-to-string (*error-output*)
                 (%save-offset 0))))
      (assert (search "[offset]" err)))))

;; 42. комбинировать с ошибкой — не бросает, логирует
(тест "комбинировать с несуществующим потоком"
  (let ((result (комбинировать '("несуществующий") "задача" :параллельно t)))
    (assert (stringp result))))

;; 43. поток-питон:очистить-код — nil → ""
(тест "поток-питон:очистить-код nil"
  (assert (string= (поток-питон:очистить-код nil) "")))

;; 44. поток-питон:очистить-код — markdown
(тест "поток-питон:очистить-код markdown"
  (let ((код (поток-питон:очистить-код (format nil "```python~%print(1)~%```"))))
    (assert (string= код "print(1)"))))

;; 45. поток-питон:очистить-код — plain
(тест "поток-питон:очистить-код plain"
  (assert (string= (поток-питон:очистить-код "print(1)") "print(1)")))

(format t "~%Итого ошибок: ~A~%" *тест-ошибки*)
(when (plusp *тест-ошибки*) (sb-ext:exit :code 1))

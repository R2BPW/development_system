;;; packages.lisp — defpackage :мастер
(defpackage #:мастер
  (:use #:cl #:uiop)
  (:nicknames #:м)
  (:export
   ;; основные внешние символы:
   #:start
   #:poll-loop
   #:обработать-команду
   #:обработать-update
   #:читать-душу #:писать-душу #:душа->системный-промпт
   #:llm-complete #:*llm-fn* #:*api-post-fn* #:с-моками
   #:get-updates #:send-message #:send-document #:answer-callback-query #:make-inline-keyboard
   #:загрузить-историю #:сохранить-историю #:добавить-сообщение #:очистить-историю #:*макс-история*
   #:список-потоков #:загрузить-поток #:загрузить-все-потоки #:запустить-поток #:активные-потоки
   #:комбинировать
   ;; логирование
   #:log/info #:log/error))

(in-package #:мастер)

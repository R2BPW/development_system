(in-package #:мастер)

(defun читать-душу (путь)
  "Читает файл души из пути и возвращает plist. Формат файла — S-expression."
  (with-open-file (in путь :direction :input)
    (read in)))

(defun писать-душу (душа путь)
  "Сохраняет plist душа в путь как read-able S-expression."
  (with-open-file (out путь :direction :output :if-exists :supersede :if-does-not-exist :create)
    (let ((*print-pretty* t))
      (prin1 душа out))))

(defun душа->системный-промпт (душа)
  "Формирует системный промпт из структуры души (plist)."
  (let ((имя (getf душа :имя))
        (описание (getf душа :описание))
        (стиль (getf душа :стиль))
        (инструкции (getf душа :инструкции)))
    (format nil "Имя: ~a~%Описание: ~a~%Стиль: ~a~%Инструкции: ~a" имя описание стиль инструкции)))

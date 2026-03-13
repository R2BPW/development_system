(in-package #:мастер)

(defun %join (sep strings)
  (if (null strings) ""
      (reduce (lambda (a b) (concatenate 'string a sep b)) strings)))

(defun комбинировать (потоки задача &key параллельно)
  "Выполнить ПОТОКИ (список имён) над ЗАДАЧЕЙ.
:параллельно — каждый поток на одну задачу, результаты объединяются через '---'.
По умолчанию — последовательно: вывод предыдущего = вход следующего."
  (if параллельно
      (%join "\n---\n" (mapcar (lambda (имя)
                                 (handler-case (or (запустить-поток имя задача) "")
                                   (error (e)
                                     (log/error "комбо" "~A: ~A" имя e) "")))
                               потоки))
      (reduce (lambda (вход имя)
                (handler-case (or (запустить-поток имя вход) вход)
                  (error (e)
                    (log/error "комбо" "~A: ~A" имя e) вход)))
              потоки :initial-value задача)))

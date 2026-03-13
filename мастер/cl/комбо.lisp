(in-package #:мастер)

(defun %join (sep strings)
  (reduce (lambda (a b) (concatenate 'string a sep b)) strings :initial-value ""))

(defun комбинировать (потоки задача &key параллельно)
  "Выполнить ПОТОКИ (список имён) над ЗАДАЧЕЙ.
:параллельно — каждый поток на одну задачу, результаты объединяются через '---'.
По умолчанию — последовательно: вывод предыдущего = вход следующего."
  (if параллельно
      (%join "\n---\n" (mapcar (lambda (имя)
                                 (or (ignore-errors (запустить-поток имя задача)) ""))
                               потоки))
      (reduce (lambda (вход имя)
                (or (ignore-errors (запустить-поток имя вход)) вход))
              потоки :initial-value задача)))

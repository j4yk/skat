(in-package :skat-utils)

(defun to-keyword (symbol)
  "Gibt ein Symbol gleichen Namens aus dem Package keyword zurück"
  (intern (symbol-name symbol) :keyword))

(defun parse-function-body (body)
  "Gibt die Forms und den Docstring zurück."
  (if (stringp (car body))
      (values (cdr body) (car body))
      (values body nil)))

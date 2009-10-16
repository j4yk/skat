(in-package :skat-utils)

(defun to-keyword (symbol)
  "Gibt ein Symbol gleichen Namens aus dem Package keyword zurück"
  (intern (symbol-name symbol) :keyword))

(defun parse-function-body (body)
  "Gibt die Forms und den Docstring zurück."
  (if (stringp (car body))
      (values (cdr body) (car body))
      (values body nil)))

(defun separate-lambda-list (lambda-list)
  "Trennt eine Lambda-Liste auf.
seperate-lambda-list (argument*) ==> normale-Parameter, key-Parameter, rest-Parameter"
  (let ((pkey (position '&key lambda-list))
	(prest (position '&rest lambda-list)))
    (values (subseq lambda-list 0 (or pkey prest))
	    (when pkey (subseq lambda-list (1+ pkey) prest))
	    (when prest (subseq lambda-list (1+ prest))))))

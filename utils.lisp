#-clunit
(defpackage org.ancar.clunit
  (:export deftest))

#-clunit
(defun org.ancar.clunit:deftest (&rest args)
  (declare (ignore args))
  (warn "no CLUNIT"))

(defpackage skat-utils
  (:nicknames :utils)
  (:use :cl :org.ancar.clunit)
  (:export to-keyword
	   conc-symbols
	   parse-function-body
	   separate-lambda-list
	   deftests
	   always-true
	   curry rcurry
	   define-package-which-exports-all-requests)) ; das steckt in requests.lisp

(in-package :skat-utils)

(defun to-keyword (symbol)
  "Gibt ein Symbol gleichen Namens aus dem Package keyword zurück"
  (intern (symbol-name symbol) :keyword))

(defun conc-symbols (&rest symbols)
  "Fügt Symbolnamen zu einem neuen Symbol zusammen und interniert es im Package"
  (intern (apply #'concatenate 'string (mapcar #'symbol-name symbols))))

(defun parse-function-body (body)
  "Gibt die Forms, Declarations und den Docstring zurück."
  (if (stringp (car body))
      (if (and (listp (cadr body)) (eq (caadr body) 'declare))
	  (values (cddr body) (car body) (cadr body))
	  (values (cdr body) (car body) nil))
      (if (and (listp (car body)) (eq (caar body) 'declare))
	  (values (cdr body) nil (car body))
	  (values body nil nil))))

(defun separate-lambda-list (lambda-list)
  "Trennt eine Lambda-Liste auf.
seperate-lambda-list (argument*) ==> normale-Parameter, key-Parameter, rest-Parameter"
  (let ((pkey (position '&key lambda-list))
	(prest (position '&rest lambda-list)))
    (values (subseq lambda-list 0 (or pkey prest))
	    (when pkey (subseq lambda-list (1+ pkey) prest))
	    (when prest (subseq lambda-list (1+ prest))))))

(defun get-args-from-specialized-lambda-list (lambda-list)
  "Returns the names of the parameters in lambda-list"
  (mapcar (lambda (arg) (if (listp arg) (car arg) arg))
	  lambda-list))

(defmacro deftests (category &body test-definitions)
  "Ermöglicht das Definieren mehrerer Unit-Tests auf einmal
und etwas bequemer.

deftests category test-definition*
test-definition ::= (name (test-function input-form*) output-form)"
  `(progn
     ,@(loop for test in test-definitions
	  collect (let ((name (first test))
			(fn-name (car (second test)))
			(input-forms (cdr (second test)))
			(output-form (third test)))
		    `(deftest ,name :category ,category
			      :input-form (values ,@input-forms)
			      :test-fn #',fn-name
			      :output-form ,output-form
			      :compare-fn #'equalp)))))

(defun always-true (&rest args)
  (declare (ignore args))
  (error "use #'constantly instead"))

(defun curry (fn &rest args-to-curry)
  "Returns a function that applies args-to-curry and the arguments supplied
to the returned function to fn."
  #'(lambda (&rest args) (apply fn (append args-to-curry args))))

(defun rcurry (fn &rest last-args-to-curry)
  "Returns a function that applies the arguments supplied to the returned
function and last-args-to-curry to fn."
  #'(lambda (&rest args) (apply fn (append args last-args-to-curry))))

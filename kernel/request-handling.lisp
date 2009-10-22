(in-package :skat-kernel)

(define-condition %request-error (error)
  ((request-name :accessor request-name :initarg :request-name)))

(define-condition wrong-request-parameters (%request-error) ())
(define-condition undefined-request-error (%request-error) ())

(defun handler-fn-name (request-name)
  "Gibt den Namen der Funktion zurück, die bei einem bestimmten Request aufgerufen wird."
  (intern (concatenate 'string (symbol-name request-name) "-HANDLER") 'skat-kernel))

(defvar *request-handlers* nil "Liste mit Assocs: request->function")

(defun handler-fn (request-name)
  "Gibt das Handlerfunktionsobjekt für diese Art Anfrage zurück."
  (cdr (assoc request-name *request-handlers*)))

(defun register-handler-fn (request-name fn)
  "Macht die Funktion fn zur Handlerfunktion für Anfragen dieses Typs."
  (push (cons request-name fn) *request-handlers*))

(defmacro defhandler (request-name (&rest states) (kernel-class-and-varname &rest request-args) &body body)
  "Definiert eine Handlerfunktion für diese Anfragen.

request-name: Name des requests
states:       Liste mit states, in denen der request verarbeitet wird
              (nil bedeutet, dass der Handler immer gilt)
player-arg:   Name des Parameters, der das Spielerobjekt enthält
sender-arg:   Name des Parameters, der die Repräsentation des request-Absenders enthält
request-args: weitere Parameter für den request
body:         forms des handlers"
  ;; prüfen, ob es diesen Anfragetyp überhaupt gibt
  (unless (requests:request-exists-p request-name)
    (error 'undefined-request-error :request-name request-name))
  ;; prüfen, ob die Parameter richtig heißen
  (unless (apply #'requests:correct-parameters-p request-name request-args)
    (error 'wrong-request-parameters :request-name request-name))
  (multiple-value-bind (forms docstring) (parse-function-body body)
    (let ((handler-fn-name (handler-fn-name request-name))
	  (encapsulated-body `(let ((kernel ,kernel-class-and-varname)
				    (request-name ',request-name))
				(declare (ignorable kernel request-name))
				,@forms)))
      `(prog1
	   ;; handler function definieren
	   (defmethod ,handler-fn-name ((,kernel-class-and-varname ,kernel-class-and-varname) sender ,@request-args)
	     ,(or docstring (format nil "Handler Funktion für Request ~a" request-name))
	     ,(if (null states) ; states = () bedeutet, Handler gilt immer
		  encapsulated-body
		  `(if (member (state ,kernel-class-and-varname) '(,@states)) ; vorher state abfragen
		       ,encapsulated-body
		       (signal 'request-state-mismatch :state (state ,kernel-class-and-varname) 
			       :request-name ',request-name :request-args ,@request-args))))
	 ;; handler function registrieren
	 (register-handler-fn ',request-name #',handler-fn-name)))))


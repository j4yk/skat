(in-package :skat-kernel)

(define-condition wrong-request-parameters (error)
  ((request-name :accessor request-name :initarg :request-name)))

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

(defmacro defhandler (request-name states (kernel-class-and-varname sender-arg &rest request-args) &body body)
  "Definiert eine Handlerfunktion für diese Anfragen.

request-name: Name des requests
states:       Liste mit states, in denen der request verarbeitet wird
player-arg:   Name des Parameters, der das Spielerobjekt enthält
sender-arg:   Name des Parameters, der die Repräsentation des request-Absenders enthält
request-args: weitere Parameter für den request
body:         forms des handlers"
  ;; prüfen, ob die Parameter richtig heißen
  (unless (apply #'requests:correct-parameters-p request-name request-args)
    (error 'wrong-request-parameters :request-name request-name))
  (let ((handler-fn-name (handler-fn-name request-name)))
    `(progn
       ;; handler function definieren
       (defmethod ,handler-fn-name ((,kernel-class-and-varname ,kernel-class-and-varname) ,sender-arg ,@request-args)
	 (if (member (state ,kernel-class-and-varname) '(,@states))
	     (progn
	       ,@body)
	     (signal 'request-state-mismatch :state (state ,kernel-class-and-varname) 
		     :request-name ',request-name :request-args ,@request-args)))
       ;; handler function registrieren
       (register-handler-fn ',request-name #',handler-fn-name)
       ;; function name zurückgeben, damit defhandler wie defmethod oder defun arbeitet
       ',handler-fn-name)))

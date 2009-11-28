(in-package :skat-kernel)

(defun handler-fn-name (request-name)
  "Gibt den Namen der Funktion zurück, die bei einem bestimmten Request aufgerufen wird."
  ;; paketbereinigt durch symbol-name
  (intern (concatenate 'string (symbol-name request-name) "-HANDLER") 'skat-kernel))

(defvar *request-handlers* nil "Liste mit Assocs: request->function")

(defun handler-fn (request-name)
  "Gibt das Handlerfunktionsobjekt für diese Art Anfrage zurück."
  ;; paketbereinigt durch to-keyword
  (cdr (assoc (to-keyword request-name) *request-handlers*)))

(defun register-handler-fn (request-name fn)
  "Macht die Funktion fn zur Handlerfunktion für Anfragen dieses Typs."
  ;; paketbereinigt durch to-keyword
  (push (cons (to-keyword request-name) fn) *request-handlers*))

(define-condition request-state-mismatch (error)
  ((state :accessor state :initarg :state)
   (request-name :accessor request-name :initarg :request-name)
   (request-args :accessor request-args :initarg :request-args))
  (:documentation "Signalisiert, dass eine Anfrage eintraf, die im aktuellen Zustand des Kernels nicht erlaubt ist."))

(define-condition error-in-handler (error)
  ((inner-condition :accessor inner-condition :initarg :error)
   (handler-function-name :accessor handler-function-name :initarg :handler-fn-name))
  (:documentation "Signalisiert, dass eine Error-Condition in einem Request-Handler auftrat."))

(define-condition error-in-kernel-handler (error-in-handler)
  ((kernel :accessor kernel :initarg :kernel))
  (:documentation "Signalisiert, dass eine Error-Condition in einem Kernel-Request-Handler auftrat."))

(defun raise-error-in-kernel-handler (kernel fn-name condition)
  "Signalisiert einen error-in-kernel-handler error."
  (restart-case (error 'error-in-kernel-handler :error condition :handler-fn-name fn-name :kernel kernel)
    (raise-inner-condition () (error condition))))

(defmacro defhandler (request-name (&rest states) allowed-senders (kernel-class-and-varname &rest request-args) &body body)
  "Definiert eine Handlerfunktion für diese Anfragen.

request-name: Name des requests
states:       Liste mit states, in denen der request verarbeitet wird
              (nil bedeutet, dass der Handler immer gilt)
allowed-senders ::= :any | sender-slot | (sender-slot*)
  sender-slot muss der Name eines Accessors für das Kernelobjekt sein, das eine Adresse zurückgibt
player-arg:   Name des Parameters, der das Spielerobjekt enthält
sender-arg:   Name des Parameters, der die Repräsentation des request-Absenders enthält
request-args: weitere Parameter für den request
body:         forms des handlers"
  (eval-when (:compile-toplevel)
    (apply #'requests:validate-request-handler request-name request-args)
    (unless (null states)
      (let ((valid-states (cdr (assoc kernel-class-and-varname *kernel-states*))))
	(if (subsetp states valid-states)
	    (format t "%Handlerdefinition ~a für ~a ist korrekt." request-name kernel-class-and-varname)
	    (error "~a sind keine definierten Zustände von ~a"
		   (set-difference states valid-states) kernel-class-and-varname)))))
  (multiple-value-bind (forms docstring) (parse-function-body body)
    (let* ((handler-fn-name (handler-fn-name request-name))
	   (allowed-senders-forms (if (listp allowed-senders)
				      (loop for attribute in allowed-senders
					 collect `(,attribute kernel)) ; in allen entsprechenden Kernel-Slots gucken
				      ;; :any muss hier nicht behandelt werden, da allowed-senders-forms in dem
				      ;; Fall unten gar nicht eingebaut wird
				      `((,allowed-senders kernel)))) ; nur in dem einen Kernelslot gucken
	   ;; let-Bindings für den Body:
	   (encapsulated-body `(let ((kernel ,kernel-class-and-varname)
				     (request-name ',request-name))
				 (declare (ignorable kernel request-name))
				 ,(if (eq :any allowed-senders)
				      `(progn ,@forms)
				      `(with-correct-sender sender ,allowed-senders-forms
					 ,@forms)))))
      `(prog1
	   ;; handler function definieren
	   (defmethod ,handler-fn-name ((,kernel-class-and-varname ,kernel-class-and-varname) sender ,@request-args)
	     ,(or docstring (format nil "Handler Funktion für Request ~a" request-name))
	     (handler-bind ((error #'(lambda (condition) (raise-error-in-kernel-handler ,kernel-class-and-varname
											',handler-fn-name condition))))
	       ,(if (null states) ; states = () bedeutet, Handler gilt immer
		    encapsulated-body
		    `(if (member (state ,kernel-class-and-varname) '(,@states)) ; vorher state abfragen
			 ,encapsulated-body
			 (signal 'request-state-mismatch :state (state ,kernel-class-and-varname) 
				 :request-name ',request-name :request-args ,@request-args)))))
	 ;; handler function registrieren
	 (register-handler-fn ',request-name #',handler-fn-name)))))

(defun call-handler-fn (kernel sender request-name &rest arguments)
  "Ruft eine Handlerfunktion auf"
  (apply (handler-fn request-name) kernel sender arguments))

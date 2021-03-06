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

(define-condition handler-not-defined-error (error)
  ((kernel :accessor kernel :initarg :kernel)
   (request-name :accessor request-name :initarg :request-name))
  (:report (lambda (condition stream)
	     (format stream "~a is not implemented for kernel class ~a"
		     (request-name condition) (class-of (kernel condition))))))

(define-condition request-state-mismatch (error)
  ((state :accessor state :initarg :state)
   (request-name :accessor request-name :initarg :request-name)
   (request-args :accessor request-args :initarg :request-args))
  (:report (lambda (condition stream)
	     (format stream "Kann ~a in Zustand ~a nicht verarbeiten!" (request-name condition) (state condition))))
  (:documentation "Signalisiert, dass eine Anfrage eintraf, die im aktuellen Zustand des Kernels nicht erlaubt ist."))

(define-condition error-in-handler (error)
  ((inner-condition :accessor inner-condition :initarg :error)
   (handler-function-name :accessor handler-function-name :initarg :handler-fn-name))
  (:documentation "Signalisiert, dass eine Error-Condition in einem Request-Handler auftrat."))

(define-condition error-in-kernel-handler (error-in-handler)
  ((kernel :accessor kernel :initarg :kernel))
  (:documentation "Signalisiert, dass eine Error-Condition in einem Kernel-Request-Handler auftrat."))

(defun raise-error-in-kernel-handler (kernel condition fn-name sender request-name args)
  "Signalisiert einen error-in-kernel-handler error."
  (restart-bind ((raise-inner-condition #'(lambda () (error condition))))
    (restart-case (error  'error-in-kernel-handler :error condition :handler-fn-name fn-name :kernel kernel)
      (push-request-back ()
	:report "put the request back into comm (this is from the kernel handler function)"
	;; Anfrage zurückpacken, damit sie nicht verloren geht
	(comm::prepend-request (comm kernel) sender request-name args)
	(error 'error-in-kernel-handler :error condition :handler-fn-name fn-name :kernel kernel)))))

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
  (multiple-value-bind (forms docstring declarations) (parse-function-body body)
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
	   (progn
	     (unless (fboundp ',handler-fn-name)
	       (defgeneric ,handler-fn-name (kernel sender ,@request-args))
	       ;; have a special error raised if the handler is undefined
	       (defmethod no-applicable-method ((handler (eql #',handler-fn-name)) &rest args)
		 (error 'handler-not-defined-error
			:kernel (car args)
			:request-name ',request-name)))
	     ;; handler function definieren
	     (defmethod ,handler-fn-name ((,kernel-class-and-varname ,kernel-class-and-varname) sender ,@request-args)
	       ,(or docstring (format nil "Handler Funktion für Request ~a" request-name))
	       ,declarations
	       (restart-case 
		   ,(if (null states) ; states = () bedeutet, Handler gilt immer
			encapsulated-body
			`(if (member (state ,kernel-class-and-varname) '(,@states)) ; vorher state abfragen
			     ,encapsulated-body
			     (error 'request-state-mismatch :state (state ,kernel-class-and-varname) 
				    :request-name ',request-name :request-args ,@request-args)))
		 (retry () :report "call the kernel handler again"
			(,handler-fn-name ,kernel-class-and-varname sender ,@request-args)))))
	 ;; handler function registrieren
	 (register-handler-fn ',request-name #',handler-fn-name)))))



(defun call-handler-fn (kernel sender request-name &rest arguments)
  "Ruft eine Handlerfunktion auf"
  (apply (handler-fn request-name) kernel sender arguments))

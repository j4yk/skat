(in-package :skat-ui)

(defun handler-fn-name (request-name)
  "Gibt den Namen der Funktion zurück, die bei einem bestimmten Request durch die UI aufgerufen wird."
  (intern (concatenate 'string (symbol-name request-name) "-HANDLER") 'skat-ui))

(defun handler-fn (request-name)
  "Gibt das UI-Handlerfunktionsobjekt für diese Art Anfrage zurück."
  (symbol-function (handler-fn-name request-name)))

(define-condition error-in-ui-handler (kernel:error-in-handler)
  ((ui :accessor ui :initarg :ui))
  (:documentation "Signalisiert, dass eine Error-Condition in einem UI-Request-Handler auftrat."))

(defun raise-error-in-ui-handler (ui fn-name condition)
  "Signalisiert einen error-in-kernel-handler error."
  (restart-case (error 'error-in-ui-handler :error condition :handler-fn-name fn-name :ui ui)
    (raise-inner-condition () (error condition))))

(defmacro defhandler (request-name (ui-class &rest parameters) &body body)
  "Definiert eine UI-Handlerfunktion für diese Anfrage."
  (apply #'requests:validate-request-handler request-name parameters)
  (let ((handler-fn-name (handler-fn-name request-name)))
    `(defmethod ,handler-fn-name ((ui ,ui-class) sender ,@parameters)
       (handler-bind ((error #'(lambda (condition) (raise-error-in-ui-handler ui ',handler-fn-name
									      condition))))
	 ,@body))))

(defun call-handler-fn (ui sender request-name &rest arguments)
  "Ruft eine UI-Handlerfunktion auf"
  (apply (handler-fn request-name) ui sender arguments))

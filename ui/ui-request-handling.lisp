(in-package :skat-ui)

(import 'kernel::validate-request-handler)

(defun handler-fn-name (request-name)
  "Gibt den Namen der Funktion zurück, die bei einem bestimmten Request durch die UI aufgerufen wird."
  (intern (concatenate 'string (symbol-name request-name) "-HANDLER") 'skat-ui))

(defun handler-fn (request-name)
  "Gibt das UI-Handlerfunktionsobjekt für diese Art Anfrage zurück."
  (symbol-function (handler-fn-name request-name)))

(defmacro defhandler (request-name (ui-class &rest parameters) &body body)
  "Definiert eine UI-Handlerfunktion für diese Anfrage."
  (apply #'validate-request-handler request-name parameters)
  `(defmethod ,(handler-fn-name request-name) ((ui ,ui-class) sender ,@parameters)
     ,@body))

(defun call-handler-fn (ui sender request-name &rest arguments)
  "Ruft eine UI-Handlerfunktion auf"
  (apply (handler-fn request-name) ui sender arguments))

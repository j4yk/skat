(in-package :skat-kernel)

(defclass base-kernel ()
  ((ui :accessor ui :initarg :ui)
   (comm :accessor comm :initarg :comm)
   (state :accessor state)
   (own-address :accessor own-address)))

(defmacro call-ui (request-name player sender &rest request-args)
  "Stellt die Anfrage an die UI weiter."
  `(apply #',(intern (symbol-name (handler-fn-name request-name)) 'skat-ui) (ui ,player) ,sender ,@request-args))

(defmethod address-compare-function ((kernel base-kernel))
  (address-compare-function (comm kernel)))

(defmethod receive-requests ((kernel base-kernel))
  "Holt alle vorliegenden Anfragen aus dem Kommunikationsobjekt heraus und ruft entsprechende Anfragehandler auf."
  (loop while (comm:has-request (comm kernel))
     do (multiple-value-bind (request-name sender request-args) (comm:get-request (comm kernel))
	  (apply (handler-fn request-name) sender request-args))))

(defmethod switch-state ((kernel base-kernel) target-state)
  "Wechselt den Zustand des Kernelobjekts. Dies hat Auswirkungen auf die Menge der Akzeptierten Anfragen."
  (setf (state kernel) target-state))

;; Comm teilt die eigene Adresse nach erfolgtem Login mit
(defhandler own-address (unregistered) (base-kernel comm address)
  (setf (own-address base-kernel) address))

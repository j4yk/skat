(in-package :skat-kernel)

(defclass kernel ()
  ((comm :accessor comm :initarg :comm)
   (state :accessor state)
   (own-address :accessor own-address)))

(defmethod address-compare-function ((kernel kernel))
  (address-compare-function (comm kernel)))

(defmethod receive-requests ((kernel kernel))
  "Holt alle vorliegenden Anfragen aus dem Kommunikationsobjekt heraus und ruft entsprechende Anfragehandler auf."
  (loop while (comm:has-request (comm kernel))
     do (multiple-value-bind (request-name sender request-args) (comm:get-request (comm kernel))
	  (apply (handler-fn request-name) sender request-args))))

(defmethod switch-state ((kernel kernel) target-state)
  "Wechselt den Zustand des Kernelobjekts. Dies hat Auswirkungen auf die Menge der Akzeptierten Anfragen."
  (setf (state kernel) target-state))

;; Comm teilt die eigene Adresse nach erfolgtem Login mit
(defhandler own-address (unregistered) (kernel comm address)
  (setf (own-address kernel) address))

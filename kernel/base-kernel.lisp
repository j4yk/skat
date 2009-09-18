(in-package :skat-kernel)

(defclass base-kernel ()
  ((comm :accessor comm :initarg :comm)
   (state :accessor state)
   (own-address :accessor own-address)))

(defmethod receive-requests ((subject base-kernel))
  "Holt alle vorliegenden Anfragen aus dem Kommunikationsobjekt heraus und ruft entsprechende Anfragehandler auf."
  (loop while (comm:has-request (comm subject))
     do (multiple-value-bind (request-name sender request-args) (comm:get-request (comm subject))
	  (apply (handler-fn request-name) sender request-args))))

(defmethod switch-state ((subject base-kernel) target-state)
  "Wechselt den Zustand des Kernelobjekts. Dies hat Auswirkungen auf die Menge der Akzeptierten Anfragen."
  (setf (state subject) target-state))

;; Comm teilt die eigene Adresse nach erfolgtem Login mit
(defhandler own-address (unregistered) (base-kernel comm address)
  (setf (own-address base-kernel) address))

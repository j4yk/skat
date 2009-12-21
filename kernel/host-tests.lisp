(in-package skat-kernel)

(defun make-test-host ()
  (let ((host (make-instance 'host :ui (make-instance 'ui:host-ui) :comm (make-instance 'comm::stub-comm) :login-data nil)))
    (setf (ui::kernel (ui host)) host)
    (comm:start (comm host))		; Comm starten
    host))

(defun make-host-test-set ()
  "Erstellt einen Host und drei Stub-Comms für Spieler"
  (let ((host (make-test-host))
	(player-comms (list (make-instance 'comm::stub-comm :id "DEALER")
			    (make-instance 'comm::stub-comm :id "LISTENER")
			    (make-instance 'comm::stub-comm :id "BIDDER"))))
    (values host player-comms)))
	 
(defun assert-state (asserted-state kernel)
  "Setzt einen bestimmten Zustand beim Spieler voraus."
  (assert (eq (state kernel) asserted-state) ()
	  "~a sollte in ~a sein, ist aber in ~a" kernel asserted-state (state kernel)))

(defun update-kernels (kernels)
  "Lässt jede UI ausstehende Anfragen verarbeiten"
  (handler-bind ((comm::stub-communication-send #'stub-communication-send-testhandler))
    (dolist (kernel kernels)
      (ui:just-one-step (ui kernel)))))

(defun stub-communication-send-testhandler (condition)
  "Condition-Handler. Stellt eine Anfrage von der einen zur anderen Stub-Comm zu
und ruft den Continue-Restart auf."
  (declare (type comm::stub-communication-send condition))
  (comm::push-request (comm::address condition)
		      (comm::sender-comm condition) (comm::request-name condition)
		      (comm::args condition))
  (continue))

(defun update-host (host)
  "Lässt den Host seine automatische Arbeit tun."
  (handler-bind ((comm::stub-communication-send #'stub-communication-send-testhandler))
    (ui:just-one-step (ui host))))

(in-package skat-kernel)

(defun make-test-player ()
  (let ((player (make-instance 'player :ui (make-instance 'ui:stub-ui) :comm (make-instance 'comm::stub-comm))))
    (setf (kernel (ui player)) player)
    player))

(defmacro -send (kernel request &rest args)
  `(ui::send-request-to-kernel (ui ,kernel) ',request ,@args))

(defmacro -do (kernel)
  `(ui:just-one-step (ui ,kernel)))

(defun player-prepared-for-registration-p (player)
  (and (slot-boundp player 'own-address)))

(defun test-game ()
  (let* ((p1 (make-test-player))
	 (p2 (make-test-player))
	 (p3 (make-test-player))
	 (host (make-test-host))
	 (received-requests `((,p1) (,p2) (,p3)))
	 (players (list p1 p2 p3)))
    (map nil #'comm:start (mapcar #'comm (list p1 p2 p3 host))) ; starte Comms
    (labels ((ship-request (condition)
	       "Stellt eine Anfrage von der einen zur anderen Stub-Comm zu."
	       (declare (type comm::stub-communication-send condition))
	       (comm::push-request (comm::address condition)
		      (comm::sender-comm condition) (comm::request-name condition)
		      (comm::args condition))
	       (continue))
	     (ui-send (receiving-kernel request &rest args)
	       "Simuliert eine Benutzeraktion"
	       (handler-bind ((comm::stub-communication-send #'ship-request)) ; Nachrichten zustellen
		 (apply #'ui::send-request-to-kernel (ui receiving-kernel) request args)))
	     (received-request-names (player)
	       (mapcar #'car (cdr (assoc player received-requests))))
	     (clear-requests (player)
	       (setf (cdr (assoc player received-requests)) nil))
	     (push-received-ui-request (condition)
	       "Merkt sich eine für die UI angekommene Anfrage"
	       (declare (type ui::stub-ui-request-arrived condition))
	       (push (ui::request-call condition) (cdr (assoc (ui::ui condition) received-requests)))
	       (continue))
	     (update-entities ()
	       "Lässt jede UI ausstehende Anfragen verarbeiten"
	       (handler-bind ((ui::stub-ui-request-arrived #'push-received-ui-request))
		 (-do host)
		 (-do p1)
		 (-do p2)
		 (-do p3))))
      (macrolet ((assert-for-each-player (form)
		   `(dolist (player players)
		      (assert ,form))))
	(update-entities) 		; erstes Mal, login-parameters muss ankommen
	(dolist (p players)
	  (assert (member 'login-parameters (received-request-names p)))
	  (clear-requests p)
	  (ui-send p 'login-data nil))
	(update-entities)		; einloggen, registration-parameters muss kommen
	(dolist (p players)		
	  (assert (member 'registration-parameters (received-request-names p)))
	  (ui-send p 'registration-data nil)))))) ; ... geht noch weiter...

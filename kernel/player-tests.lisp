(in-package skat-kernel)

(defun make-test-player ()
  (let ((player (make-instance 'player :ui (make-instance 'ui:stub-ui) :comm (make-instance 'comm::stub-comm))))
    (setf (ui::kernel (ui player)) player)
    player))

(defmacro -send (kernel request &rest args)
  `(ui::send-request-to-kernel (ui ,kernel) ',request ,@args))

(defmacro -do (kernel)
  `(ui:just-one-step (ui ,kernel)))

(defun player-prepared-for-registration-p (player)
  (and (slot-boundp player 'own-address)))

(defun assert-state (asserted-state player)
  "Setzt einen bestimmten Zustand beim Spieler voraus."
  (assert (eq (state player) asserted-state) (player (state player) asserted-state)
	  "Spieler ~a sollte in ~a sein, ist aber in ~a" player asserted-state (state player)))

(defun test-game-before-bidding ()
  (let* ((p1 (make-test-player))
	 (p2 (make-test-player))
	 (p3 (make-test-player))
	 (host (make-test-host))
	 (received-requests `((,(ui p1)) (,(ui p2)) (,(ui p3))))
	 (players (list p1 p2 p3)))
    (map nil #'comm:start (mapcar #'comm (list p1 p2 p3 host))) ; starte Comms
    (labels (
	     ;; Restart Funktionen für stub-send und ui-received
	     (ship-request (condition)
	       "Stellt eine Anfrage von der einen zur anderen Stub-Comm zu."
	       (declare (type comm::stub-communication-send condition))
	       (comm::push-request (comm::address condition)
		      (comm::sender-comm condition) (comm::request-name condition)
		      (comm::args condition))
	       (continue))
	     (push-received-ui-request (condition)
	       "Merkt sich eine für die UI angekommene Anfrage"
	       (declare (type ui::stub-ui-request-arrived condition))
	       (push (ui::request-call condition) (cdr (assoc (ui::ui condition) received-requests)))
	       (continue))
	     
	     (ui-send (receiving-kernel request &rest args)
	       "Simuliert eine Benutzeraktion"
	       (handler-bind ((comm::stub-communication-send #'ship-request)) ; Nachrichten zustellen
		 (apply #'ui::send-request-to-kernel (ui receiving-kernel) request args)))

	     (received-request-names (player)
	       "Gibt die Namen aller empfangenden Anfragen (UI) zurück."
	       (let ((requests (cdr (assoc (ui player) received-requests))))
		 (if (null requests)
		     nil
		     (mapcar #'car requests))))
	     (assert-received (request player)
	       "Setzt voraus, dass eine bestimmte Anfrage bei der UI angekommen ist."
	       (assert (member request (received-request-names player))))
	     (clear-requests (player)
	       (setf (cdr (assoc (ui player) received-requests)) nil))

	     (update-entities ()
	       "Lässt jede UI ausstehende Anfragen verarbeiten"
	       (handler-bind ((ui::stub-ui-request-arrived #'push-received-ui-request)
			      (comm::stub-communication-send #'ship-request))
		 (-do host)
		 (-do p1)
		 (-do p2)
		 (-do p3))))
      (update-entities) 		; erstes Mal, login-parameters muss ankommen
      (dolist (p players)
	(assert-received 'ui:login-parameters p)
	(assert-state 'start p)		; alle in Start
	(clear-requests p)
	(ui-send p 'login-data nil))
      (update-entities)		; einloggen, registration-struct muss kommen
      (dolist (p players)
	(assert-state 'unregistered p)
	(assert-received 'ui::registration-struct p)
	(ui-send p 'registration-data (comm::make-stub-registration-data :host-comm (comm host))) ; registieren
	(assert-state 'registration-pending p))
      (update-entities)		; hierbei müsste Host antworten und die Leute registrieren
      (dolist (p players)
	(assert-state 'registration-succeeded p) ; Registrierung war wohl erfolgreich
	(assert (slot-boundp p 'host)) ; Host muss bekannt sein
	(assert-received 'ui:registration-reply p)
	(assert-received 'ui:playmates p) ; Mitspieler wurden verkündet
	(assert (slot-boundp p 'left-playmate))
	(assert (slot-boundp p 'right-playmate))
	(ui-send p 'game-start))	; Spiel starten
      (update-entities)		; Host startet Spiel, teilt Karten aus und benennt Reizrollen
      (let ((bidder nil)
	    (listener nil))
	(dolist (p players)
	  (assert-received 'ui:game-start p) ; vom Host
	  (assert-received 'ui:cards p) ; Karten erhalten
	  (assert (slot-boundp p 'cards))
	  (let ((requests (received-request-names p)))
	    ;; Zustände je nach Reizrolle
	    (cond ((member 'ui:start-bidding requests)
		   (setf bidder p)
		   (assert-state 'bid p))
		  ((member 'ui:listen requests)
		   (setf listener p)
		   (assert-state 'listen p))
		  (t (assert-state 'bidding-wait p)))))
	(assert (not (null bidder)))	; Hörer und Sager müssen benannt worden sein
	(assert (not (null listener)))
	(assert (eq (bidding-mate listener) (own-address bidder))) ; und sie müssen sich gegenseitig kennen
	(assert (eq (bidding-mate bidder) (own-address listener)))
	(values players host)))))

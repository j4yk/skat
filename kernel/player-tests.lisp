(defpackage skat-player-test
  (:use :cl :kern))

(in-package skat-player-test)

;; Konventionen:
;; alle eigentlichen Testfunktionen nehmen als Parameter:
;; 1. eine Liste mit drei Spielerkernen
;; 2. einen Hostkern
;; und sie geben diese beiden als values auch wieder zurück

(defun make-test-player ()
  (let ((player (make-instance 'player :ui (make-instance 'ui:stub-ui) :comm (make-instance 'comm::stub-comm))))
    (setf (ui::kernel (ui player)) player)
    player))

(defmacro -send (kernel request &rest args)
  `(ui::send-request-to-kernel (ui ,kernel) ',request ,@args))

(defmacro -do (kernel)
  `(ui:just-one-step (ui ,kernel)))

(defun assert-state (asserted-state player)
  "Setzt einen bestimmten Zustand beim Spieler voraus."
  (assert (eq (state player) asserted-state) (player (state player) asserted-state)
	  "Spieler ~a sollte in ~a sein, ist aber in ~a" player asserted-state (state player)))

(defun stub-communication-send-testhandler (condition)
  "Condition-Handler. Stellt eine Anfrage von der einen zur anderen Stub-Comm zu
und ruft den Continue-Restart auf."
  (declare (type comm::stub-communication-send condition))
  (comm::push-request (comm::address condition)
		      (comm::sender-comm condition) (comm::request-name condition)
		      (comm::args condition))
  (continue))

(defun ui-send (receiving-kernel request &rest args)
  "Simuliert eine Benutzeraktion bei der UI des Kernels für den Test"
  (handler-bind ((comm::stub-communication-send #'stub-communication-send-testhandler)) ; Nachrichten zustellen
    (apply #'ui::send-request-to-kernel (ui receiving-kernel) request args)))

(defun ui-received-request-names (player)
  "Gibt die Namen aller empfangenen Anfragen der UI des Kernels zurück."
  (mapcar #'car (ui::received-requests (ui player))))

(defun assert-received (request player)
  "Setzt voraus, dass eine bestimmte Anfrage bei der UI angekommen ist"
  (assert (member request (ui-received-request-names player))))

(defun init-test-set ()
  "Erstellt einen Satz Spieler mit einem Host"
  (let ((players (list (make-test-player) (make-test-player) (make-test-player)))
	(host (make-test-host)))
    (map nil #'comm:start (mapcar #'comm (cons host players))) ; starte Comms
    (values players host)))

(defun test-game-before-bidding ()
  (multiple-value-bind (players host) (init-test-set)
    (labels ((clear-requests (player)
	       (setf (ui::received-requests (ui player)) nil))
	     (update-entities ()
	       "Lässt jede UI ausstehende Anfragen verarbeiten"
	       (handler-bind ((comm::stub-communication-send #'stub-communication-send-testhandler))
		 (dolist (kernel (cons host players))
		   (ui:just-one-step (ui kernel))))))
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
	  (let ((requests (ui-received-request-names p)))
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

(in-package skat-kernel)

(defkernel host (start registration
		       bidding-1 bidding-2 bidding-3
		       declarer-found skat-away await-declaration
		       in-game game-over)
  ((login-data :initarg :login-data :documentation "login-Parameter, die beim Initialisieren übergeben werden, damit man sie nicht mehr abfragen muss.")
   (registered-players :accessor registered-players :initform nil)
   (dealers :accessor dealers :documentation "ringlist of players, dealer is car")
   (bidding-values :accessor bidding-values :documentation "verbliebene mögliche Reizwerte")
   (current-listener :accessor current-listener :documentation "Adresse des aktuell hörenden Spielers")
   (current-bidder :accessor current-bidder :documentation "Adresse des aktuell sagenden Spielers")
   (current-declarer :accessor current-declarer :documentation "Adresse des aktuellen Spielführers")
   (score-table :accessor score-table :documentation "die Punktetabelle aus (cons Adresse Punktzahl)")
   (skat :accessor skat :documentation "Noch nicht ausgegebener Skat")
   (jacks :accessor jacks :initform nil :documentation "Buben, die der Declarer gespielt hat.")
   (declaration :accessor declarer-declaration :documentation "was der Spielführer angesagt hat")
   (table :accessor table :documentation "Ringliste der drei Spieler, wird fürs Kartenspielen gedreht")
   (tricks :accessor tricks :initform nil :documentation "Gespielte Stiche")
   (current-trick :accessor current-trick :documentation "Der aktuell auf dem Tisch liegende Stich")
   (want-game-start :accessor want-game-start :initform nil 
		    :documentation "Liste der Spieler, die eine neue Runde wollen")))

(defmethod turn-table-to ((host host) player)
  "Drehe die Ringliste im Slot table so lange, bis player car ist."
  (loop until (funcall (address-compare-function host) (car (table host)) player)
     do (setf (table host) (cdr (table host)))))

(defmethod send-to-players ((host host) request-name &rest request-args)
  "Sendet einen request an alle registrierten Spieler."
  (dolist (receiver (registered-players host))
    (apply #'comm:send (comm host) receiver request-name request-args)))

(defun shuffle (list)
  "Gibt die Liste mit weitesgehend zufällig veränderter Reihenfolge der Elemente zurück."
  (if (null list)
      nil
      (let ((card (nth (random (length list)) list)))
	(cons card (shuffle (remove card list :count 1))))))

(deftest "shuffle-test" :category "Host"
	 :test-fn #'(lambda ()
		      (let* ((list (loop for i from 0 to 10 collect (random 10)))
			     (shuffled-list (shuffle list)))
			(assert (= (length list) (length shuffled-list)))
			(dolist (item list)
			  (assert (member item shuffled-list))
			  (assert (= (count item shuffled-list) (count item list))))
			t)))

(defmethod current-dealer ((host host))
  (first (dealers host)))

(defmethod current-forehand ((host host))
  (second (dealers host)))

(defmethod current-middlehand ((host host))
  (third (dealers host)))

(defmethod reset-bidding-values ((host host))
  "Setzt die verbliebenen Reizwerte auf den Anfangszustand (also alle Werte ab 18) zurück."
  (setf (bidding-values host) (cut-away-game-point-levels 18)))

(defmethod receive-requests ((host host))
  "Entschärft invalid-request-sender-error, indem immer Gemecker zurückgeschickt wird."
  (handler-bind ((invalid-request-sender-error
		  #'(lambda (condition)
		      (comm:send (comm host) (sender condition) 'message
				 (format nil "You are not allowed to send me ~a" (request-name condition))))))
    (call-next-method)))

(define-state-switch-function registration (host reset-registered-players-p)
  "Geht in den Registrierungen-Entgegennahme Modus über.
In diesem Zustand werden Registrierungsanfragen aufgenommen."
  (when reset-registered-players-p	; Spielerliste zurücksetzen, wenn gewünscht
    (setf (registered-players host) nil)
    (slot-makunbound host 'dealers))	; löse die Tischrunde auf, wenn es schon eine gab
  (setf (want-game-start host) nil))	; alle Registrierte müssen auf Start drücken)

;; state: start. Alles vor dem Registrieren.

(define-condition no-login-data-supplied-error (error)
  ((host :accessor host :initarg :host))
  (:documentation "Wird signalisiert, wenn dem Hostobjekt keine Logindaten zur Verfügung gestellt wurden."))

(defhandler login-parameters (start) comm (host parameters)
  "Von der Kommunikation kommende Parameter zum Einwählen ins Kommunikationsmedium.
Beim Host müssen die Login-Daten schon beim Initialisieren übergeben worden sein."
  (if (slot-boundp host 'login-data)
      (progn
	(comm:login (comm host) (slot-value host 'login-data))
	(switch-to-registration host t))
      (error 'no-login-data-supplied-error :host host)))

(defhandler registration-parameters () comm (host parameters)
  "Da Comm nichts vom Host weiß, schickt sie dem Host auch die Registrierungsparameter.
Host ignoriert diese einfach.")

;; state: registration, alles

(defhandler registration-request () :any (host)
  "Behandelt Anfragen von Spielern, ob sie sich an den Tisch setzen dürfen"
  (symbol-macrolet ((accept (comm:send (comm host) sender 'registration-reply t))
		    (decline (comm:send (comm host) sender 'registration-reply nil)))
    (with-slots (registered-players) host
      (case (state host)
	(registration
	 ;; während der Registrierungsphase werden Registrierungen akzeptiert
	 (if (member sender registered-players :test (address-compare-function host))
	     ;; der Witzbold ist schon registriert
	     accept
	     (if (>= (length registered-players) 3)
		 decline
		 (progn
		   (send-to-players host 'server-update `(:player-join ,sender))
		   (push sender registered-players) ; Spieler aufnehmen
		   accept
		   (when (= (length registered-players) 3)
		     (with-slots (dealers) host
		       (setf dealers (make-ring registered-players)) ; setze die Spieler an einen runden Tisch
		       ;; und zeige ihnen entsprechend ihre Sitznachbarn
		       (comm:send (comm host) (car dealers) 'playmates (cadr dealers) (caddr dealers))
		       (comm:send (comm host) (cadr dealers) 'playmates (caddr dealers) (cadddr dealers))
		       (comm:send (comm host) (third dealers) 'playmates (fourth dealers) (fifth dealers))))))))
	(otherwise
	 ;; es werden keine Registrierungen akzeptiert
	 (comm:send (comm host) sender 'registration-reply nil)
	 (comm:send (comm host) sender 'message "Host is not in registration mode."))))))

(defhandler unregister () :any (host)
  "Behandelt die Nachricht eines Spielers, dass er die Runde verlässt."
  (when (member sender (registered-players host) :test (address-compare-function host))
    (ecase (state host)
      (registration
       ;; Spieler aus der Liste entfernen
       (setf (registered-players host)
	     (delete sender (registered-players host) :test (address-compare-function host)))
       (slot-makunbound host 'dealers)	; die Tischrunde auflösen
       (inform-players-of-leaving-player)
       (send-to-players host 'message (format nil "Spieler ~a verlässt die Runde." sender))))))

;; game-start wird ansonsten vorrangig im game-over state behandelt
;; deshalb steht der Handler unten

;; state: bidding-1. Erste Stufe des Reizens
;;        bidding-2. Zweite Reizrunde
;;        bidding-3. Dritte Reizrunde (Ramschentscheidung)

(defkernelmethod listener (host listener bidder)
  "Nominiert einen Hörer und sendet ihm den entsprechenden Auftrag."
  (setf (current-listener host) listener)
  (comm:send (comm host) listener 'listen bidder))

(defkernelmethod bidder (host bidder listener min-value)
  "Nominiert einen Reizansager und sendet ihm den entsprechenden Auftrag."
  (setf (current-bidder host) bidder)
  (comm:send (comm host) bidder 'start-bidding listener min-value))

(defmacro listen-to (listener bidder min-value)
  "Sendet zwei Spielern die Befehle zum gegenseitigen Reizen."
  `(progn
     (listener host ,listener ,bidder)
     (bidder host ,bidder ,listener ,min-value)))

(defmethod init-score-table ((host host))
  "Initialisiert die Punktetabelle, sofern sie noch nicht besteht"
  (with-slots (registered-players score-table) host
    (unless (slot-boundp host 'score-table)
      (setf score-table (make-hash-table :test (address-compare-function host)))
      (setf (gethash (first registered-players) score-table) 0
	    (gethash (second registered-players) score-table) 0
	    (gethash (third registered-players) score-table) 0)))
  (values))

(define-state-switch-function bidding-1 (host)
  "Startet das eigentliche Spiel, teilt die Karten aus und startet den Reizvorgang."
  (with-slots (registered-players skat dealers bidding-values) host
    (assert (= (length registered-players) 3) (registered-players))
    (init-score-table host)
    (send-to-players host 'game-start)	; das Spiel beginnt
    (let ((cards (shuffle (all-cards))))
      ;; Karten austeilen
      (setf skat (subseq cards 0 2))
      (comm:send (comm host) (first registered-players) 'cards (subseq cards 2 12))
      (comm:send (comm host) (second registered-players) 'cards (subseq cards 12 22))
      (comm:send (comm host) (third registered-players) 'cards (subseq cards 22 32)))
    ;; den Geber verschieben
    (setf dealers (cdr dealers))
    ;; ersten Reizauftrag erteilen: Mittelhand sagt Vorderhand
    (symbol-macrolet ((current-forehand (current-forehand host)) ; with-slots geht nicht, 
		      (current-middlehand (current-middlehand host))) ; weil dies keine Slots sind
      (listen-to current-forehand current-middlehand 18))
    (reset-bidding-values host)
    ;; und jetzt warte auf pass
    ))

(define-state-switch-function bidding-2 (host listener bidder)
  "Zweite Reizinstanz starten.
bidder (normalerweise current-dealer) sagt listener weiter."
  (listen-to listener bidder (car (bidding-values host))))

(define-state-switch-function bidding-3 (host bidder)
  "Wechselt den Host in den Zustand bidding-3.
Vorderhand darf entscheiden, ob geramscht wird oder nicht."
  ;; der Spieler soll entscheiden, ob er 18 reizt oder geramscht werden soll
  (bidder host bidder nil (car (bidding-values host)))
  (slot-makunbound host 'current-listener))

(defhandler bid (bidding-1 bidding-2 bidding-3) current-bidder (host value)
  "Behandelt das Ansagen eines Reizwertes durch einen Spieler."
  ;; Reizwerte aktualisieren
  (setf (bidding-values host) (cut-away-game-point-levels value (bidding-values host)))
  ;; Spieler erhebt Anspruch auf Spielführung
  (setf (current-declarer host) sender)
  (case (state host)
    (bidding-3
     ;; Vorderhand reizt in dritter Instanz 18, d. h. dieser Spieler wird Spielführer
     (switch-to-declarer-found host))))

(defhandler join (bidding-1 bidding-2 bidding-3) current-listener (host value)
  "Behandelt das Mitgehen eines Spielers bei einem Reizwert."
  ;; Spieler erhebt durch Mitgehen Anspruch auf Spielführung
  (setf (current-declarer host) sender))

(defmacro player-case (player &body cases)
  "(player-case {address}
  (current-dealer {form}*)?
  (current-forehand {form}*)?
  (current-middlehand {form}*)?)"
  `(cond
     ,@(loop for case in cases
	  collect `((funcall (address-compare-function host) ,player (,(car case) host))
		    ,@(cdr case)))
     (t (error 'invalid-sender))))

(defhandler pass (bidding-1 bidding-2 bidding-3) (current-listener current-bidder) (host value)
  "Behandelt das Passen eines Mitspielers bei einem Reizwert."
  (ecase (state host)
    (bidding-1				; erster Pass
     (player-case sender
       (current-forehand 		; Vorderhand hat gepasst
	;; Geber sagt Mittelhand weiter
	(switch-to-bidding-2 host (current-middlehand host) (current-dealer host)))
       (current-middlehand		; Mittelhand hat gepasst
	;; Geber sagt Vorderhand weiter
	(switch-to-bidding-2 host (current-forehand host) (current-dealer host)))))
    (bidding-2				; zweiter Pass
     (player-case sender
       (current-forehand		; Vorderhand hat gepasst
	;; d. h. Geber spielt
	(switch-to-declarer-found host))
       (current-middlehand		; Mittelhand hat gepasst
	;; d. h. Geber spielt
	(switch-to-declarer-found host))
       (current-dealer			; Geber hat gepasst
	(if (not (slot-boundp host 'current-declarer))
	    ;; noch hat keiner etwas gereizt, d. h. Vorderhand entscheidet über Ramsch
	    (switch-to-bidding-3 (current-forehand host))
	    ;; es hat schon jemand etwas gereizt und nicht gepasst, derjenige spielt
	    (switch-to-declarer-found host)))))
    (bidding-3				; dritter Pass => Ramsch
     (ramschen))))
	  
;; state: declarer-found. Warte auf hand-decision.

(define-state-switch-function declarer-found (host)
  "Der Spielführer steht nun fest, kündigt ihn an."
  (send-to-players host 'declarer (current-declarer host)))

(defhandler hand-decision (declarer-found) current-declarer (host hand)
  "Behandelt die Ansage, ob der Declarer Hand spielt und geht in den entsprechenden Folgezustand über"
  (if hand
      (switch-to-await-declaration host)		; warte gleich auf die Ansage
      (switch-to-skat-away host)))			; verschicke den Skat

;; state: skat-away. Warte auf Rückgabe des Skats.

(define-state-switch-function skat-away (host)
  "Gibt den Skat zum Spielführer und wechselt in den skat-away Zustand."
  (comm:send (comm host) (current-declarer host) 'skat (skat host)) ; Skat verschicken
  (slot-makunbound host 'skat))					    ; der Host hat den dann nicht mehr

(defhandler skat (skat-away) current-declarer (host skat)
  "Behandelt die Rückgabe des Skats vom Declarer."
  (setf (skat host) skat)		; Skat nehmen
  (switch-to-await-declaration host)) ; Ansage abwarten

;; state: await-declaration. Warte auf die Ansage des Declarers.

(define-state-switch-function await-declaration (host))

;; (defhandler flush-run (await-declaration) (host with-or-without run-value)
;;   "Behandelt die Information des Spielführers über die vorhandenen oder
;; fehlenden Trumpfspitzen."
;;   (setf (flush-run host) (cons with-or-without run-value)))

(defmethod flush-run-value ((host host))
  "Gibt die Anzahl der fehlenden oder vorhandenen Trumpfspitzen des Spielführers zurück."
  (cadr (jacks-flush-run (jacks host))))

(defhandler declaration (await-declaration) current-declarer (host declaration)
  "Behandelt die Verkündung der Ansage des Declarers."
  (switch-to-in-game host))				; starte das Stichespielen

;; state: in-game. Das Spiel läuft, die Stiche werden mitgenommen

(define-state-switch-function in-game (host declaration)
  "Startet das Stichespielen"
  (setf (declarer-declaration host) declaration) ; merke sie dir für die Stichauswertungen
  (setf (table host) (dealers host) ; die Runde aufmachen(jacks host) nil)
	(jacks host) nil	    ; gesammelte Buben zurücksetzen
	(tricks host) nil)	    ; Stiche zurücksetzen
  (turn-table-to host (current-forehand host)) ; und Vorderhand fängt an
  (comm:send (comm host) (current-forehand host) 'choose-card)) ; lässt Vorderhand anspielen

(defkernelmethod remember-jack (host jack-suit)
  "Merkt sich den gespielten Buben für die Flush-Run Auszählung"
  (with-slots (jacks) host
    (push jack-suit jacks)))

(defkernelmethod current-player (host)
  "Gibt die Adresse des Spielers zurück, der die nächste Karte spielen muss."
  (car (table host)))

(defhandler card (in-game) current-player (host card)
  "Behandelt das Spielen einer Karte durch einen Spieler."
  ;; Karte muss vom Spieler kommen, der an der Reihe ist
  (when (and (jackp card)
	     (funcall (address-compare-function host) sender (current-declarer host)))
    ;; Spielführer spielt Buben, merke dir das für den Flush-Run
    (remember-jack host (suit card)))
  (unless (slot-boundp host 'current-trick)
    (setf (current-trick host) (make-trick))) ; einen neuen Stich eröffnen
					; wenn es noch keinen gibt
  (with-slots (current-trick tricks) host
    (add-contribution card sender current-trick) ; die Karte zum Stich packen
    (setf (table host) (cdr (table host)))   ; der nächste ist dran
    (when (trick-complete-p current-trick)
      (let ((trick-winner (trick-winner current-trick
					(game-variant (declarer-declaration host)))))
	;; über fertige Stiche werden die Spieler benachrichtigt
	(send-to-players host 'trick (cards current-trick) trick-winner)
	;; auf den Stapel packen
	(push current-trick tricks)
	;; Platz machen für den nächsten Stich
	(slot-makunbound host 'current-trick)
	(if (= 10 (length tricks))	; war das der letzte Stich?
	    (switch-to-game-over host t) ; dann beende das Spiel
	    ;; nein? dann sage dem Stichsieger, dass er anspielen möge
	    ;; und schiebe ihn an die Tischfront
	    (progn
	      (turn-table-to host trick-winner)
	      (comm:send (comm host) trick-winner 'choose-card)))))))

;; state: game-over. Das Spiel ist vorbei

(defun count-card-points (tricks declarer &optional (address-compare-function #'equalp))
  "Zählt die Augen in den Stichen der beiden Spielparteien aus.
==> declarer-card-points, defenders-card-points"
  (multiple-value-bind (declarer-tricks defenders-tricks) ; Stiche teilen
      (loop for trick in tricks
	 if (funcall address-compare-function (trick-winner trick) declarer)
	 collect trick into declarer-tricks
	 else collect trick into defenders-tricks
	 end
	 finally (return (values declarer-tricks defenders-tricks)))
    ;; Punkte aufsummieren
    (values-list (mapcar #'(lambda (tricks) (apply #'+ (mapcar #'trick-card-points tricks)))
			 (list declarer-tricks defenders-tricks)))))

(defmethod send-score-table ((host host))
  "Übermittelt allen Spielern den aktuellen Punktestand"
  (with-slots (registered-players score-table) host
    (send-to-players host 'score-table
		     (first registered-players)
		     (gethash (first registered-players) score-table)
		     (second registered-players)
		     (gethash (second registered-players) score-table)
		     (third registered-players)
		     (gethash (third registered-players) score-table))))		     

(define-state-switch-function game-over (host prompt)
  "Spiel beenden und auswerten."
  (send-to-players host 'game-over prompt) ; Spieler in Kenntnis setzen
  (setf (want-game-start host) nil) ; setze die Liste der Spielwilligen zurück
  (multiple-value-bind (declarer-score defenders-score) ; Augen auszählen
      (count-card-points (tricks host) (current-declarer host) (address-compare-function host))
    (send-to-players host 'cards-score declarer-score defenders-score) ; und verschicken
    ;; nun die Augen auswerten...
    (with-slots (jacks declaration current-declarer score-table) host
      (let ((won (> declarer-score defenders-score)))
	(if won
	    (progn
	      (when (>= declarer-score 90) ; Schneider
		(push :played-schneider (cdr declaration))
		(when (= declarer-score 120)
		  (push :played-schwarz (cdr declaration)))) ; Schwarz
	      (let ((game-points (game-points declaration (flush-run-value host))))
		(send-to-players host 'game-result (append (jacks-flush-run jacks) declaration) won game-points)
		(incf (gethash current-declarer score-table) game-points)))
	    (progn
	      (when (<= declarer-score 30)
		(push :played-schneider (cdr declaration)) ; Schneider
		(when (= declarer-score 0)
		  (push :played-schwarz (cdr declaration)))) ; Schwarz
	      (let ((game-points (* 2 (game-points declaration (flush-run-value host)))))
		(send-to-players host 'game-result (append (jacks-flush-run jacks) declaration) won game-points)
		(decf (gethash current-declarer score-table) game-points)))))))
  (send-score-table host))

(defhandler game-start (registration game-over) :any (host)
  "Behandelt den Wunsch eines Spielers nach einem weiteren Spiel."
  (unless (member sender (want-game-start host) :test (address-compare-function host))
    (when (member sender (registered-players host) :test (address-compare-function host))
      (push sender (want-game-start host)))) ; vermerke, dass der Spieler game-start gesendet hat
  (if (null (set-difference (registered-players host) (want-game-start host)
			    :test (address-compare-function host)))
      (switch-to-bidding-1 host)))    ; wenn alle Spieler fertig sind, Reizen starten


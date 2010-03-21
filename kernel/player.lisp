(in-package skat-kernel)

(defkernel player
    (start
     unregistered registration-pending registration-succeeded
     bidding-wait bid listen preparations
     in-game play-cards trick-full game-over)     
  ((cards :accessor cards :documentation "Die Karten, die der Spieler auf der Hand hält")
   (current-trick :accessor current-trick :documentation "Liste der momentan auf dem Tisch liegenden Karten")
   (won-tricks :accessor won-tricks :documentation "Liste von Kartenlisten, die die gewonnenen Stiche sind")
   (bidding-values :accessor bidding-values :documentation "Liste verbliebener Reizwerte")
   (bidding-mate :accessor bidding-mate :documentation "Adresse des momentanen Reizpartners")
   (declarer :accessor declarer :documentation "Adresse des Spielführers")
   (game-declaration :accessor game-declaration :documentation "Ansage des Spielführers")
   (host :accessor host :documentation "Adresse des Hosts")
   (left-playmate :accessor left-playmate :documentation "Adresse des linken Mitspielers")
   (right-playmate :accessor right-playmate :documentation "Adresse des rechten Mitspielers")
   (table :accessor table :documentation "Ringliste mit den Adressen der drei Spieler in Ausspielreihenfolge"))
  (:documentation "Spielerobjekt. Hält alle Komponenten zusammen."))

(defmethod turn-table ((player player))
  "Bewegt die Ringliste in table einen Schritt weiter."
  (setf (table player) (cdr (table player))))

(defmethod turn-table-to ((player player) target-player)
  "Bewegt die Ringliste in Table zum angegebenen Spieler"
  (do ((counter 0 (1+ counter)))
      ((address-equal player (current-player player) target-player))
    (turn-table player)
    (if (> counter 3)
	(error "~a spielt nicht mit!" target-player))))

(deftests "Player"
  ("turn-table" ((lambda () (let ((player (make-instance 'player)))
			      (setf (table player) (make-ring '(1 2 3)))
			      (turn-table player)
			      (values (first (table player))
				      (second (table player))
				      (third (table player))))))
		(values 2 3 1)))

(defmethod current-player ((player player))
  "Gibt die Adresse des Spielers zurück, dessen Aufgabe es zur Zeit ist, ein Karte zu spielen."
  (car (table player)))

(defmethod send-to-all-others ((player player) request-name &rest request-args)
  "Ruft comm:send mit gleicher Anfrage für Host, linken und rechten Mitspieler auf."
  (dolist (receiver (list (left-playmate player) (right-playmate player) (host player)))
    (apply #'comm:send (comm player) receiver request-name request-args)))

(defmethod (setf cards) (cards (player player))
  "Sortiert Karten automatisch nach jeder Zuweisung"
  (if (slot-boundp player 'game-declaration)
      (setf (slot-value player 'cards) (sort-cards cards (game-variant (game-declaration player))))
      (setf (slot-value player 'cards) (sort-cards cards :grand))))

(DEFTEST
    "sort cards on (setf (cards player)) w/o declaration"
    :CATEGORY "Player"
    :TEST-FN #'(LAMBDA ()
		 (LET ((PLAYER (MAKE-INSTANCE 'PLAYER)))
		   (SETF (CARDS PLAYER) (print (list #!CA #!DA #!HJ #!D7)))
		   (print (cards player))))
    :OUTPUT-FORM '(#!D7 #!DA #!CA #!HJ) :compare-fn #'equalp)

(DEFTEST
    "sort cards on (setf (cards player)) w/ declaration"
    :CATEGORY "Player"
    :TEST-FN #'(LAMBDA ()
		 (LET ((PLAYER (MAKE-INSTANCE 'PLAYER)))
		   (SETF (GAME-DECLARATION PLAYER) '(:NULL :HAND :OUVERT))
		   (SETF (CARDS PLAYER) (list #!CA #!DA #!HJ #!D7))
		   (cards player)))
    :OUTPUT-FORM '(#!D7 #!DA #!HJ #!CA) :compare-fn #'equalp)

;;;;  Handler und State-Switch-Functions

(defhandler login-struct (start) comm (player struct-classname)
   "Behandelt die Loginparameterliste der Comm. Gibt sie an die UI weiter."
   (call-ui 'login-struct player sender struct-classname))

(define-state-switch-function unregistered (player)
  "Wechselt den Zustand zu unregistered."
  (SLOT-MAKUNBOUND PLAYER 'HOST))

(defhandler login-data (start) ui (player data)
  "Soll durch die UI aufgerufen werden.
Weist Comm an sich mit den Daten einzuloggen."
  (comm:login (comm player) data)
  (switch-to-unregistered player))

(DEFHANDLER REGISTRATION-struct (UNREGISTERED) comm (PLAYER struct-classname)
  "Behandelt die Parameterliste für die Registrierung von der Comm."
  (LET ((COMM SENDER))
    (CALL-UI 'REGISTRATION-struct PLAYER COMM struct-classname)))

(define-state-switch-function registration-pending (player)
  "Wechelst in den Zustand registration-pending.")

;;; == Warum Player die Host-Adresse nicht bei der Anfrage speichert ==
;; Player hat keine Information darüber, wie die Registrierungsdaten
;; zu interpretieren sind. Dies obliegt der Kommunikation.
;; == ENDE ==

(DEFHANDLER REGISTRATION-DATA (UNREGISTERED) ui (PLAYER DATA)
  "Soll durch die UI aufgerufen werden.
Weist comm an sich mit den Daten bei einem Host zu registrieren."
  (SKAT-COMMUNICATION:REGISTER (COMM PLAYER) DATA)
  (switch-to-registration-pending player))

(define-state-switch-function registration-succeeded (player host)
  "Wechelt in den Zustand registration-succeeded."
  (setf (host player) host))

(DEFHANDLER REGISTRATION-REPLY (REGISTRATION-PENDING) :any (PLAYER ACCEPTED)
  "Behandelt die Antwort auf die Registrierungsanfrage vom Host."
  (IF ACCEPTED
      (switch-to-registration-succeeded player sender)
      (switch-to-unregistered player))
  (CALL-UI 'registration-reply PLAYER SENDER ACCEPTED))

(DEFHANDLER SERVER-UPDATE (REGISTRATION-SUCCEEDED) host (PLAYER EVENTS)
  "Behandelt Neuigkeiten vom Host."
  (CALL-UI 'SERVER-UPDATE PLAYER SENDER EVENTS))

(DEFHANDLER UNREGISTER () ui (PLAYER)
  "Soll von der UI aufgerufen werden, wenn der Spieler eine Loslösung
vom Host wünscht."
  (SKAT-COMMUNICATION:SEND (COMM PLAYER) (HOST PLAYER) 'UNREGISTER)
  (switch-to-unregistered player))

(DEFHANDLER PLAYMATES (REGISTRATION-SUCCEEDED) host (PLAYER LEFT RIGHT)
  "Behandelt die Bekanntmachung der Mitspieler durch den Host."
  (SETF (LEFT-PLAYMATE PLAYER)
	LEFT
	(RIGHT-PLAYMATE PLAYER)
	RIGHT
	(TABLE PLAYER)
	(MAKE-RING (LIST (OWN-ADDRESS PLAYER) LEFT RIGHT)))
  (CALL-UI 'PLAYMATES PLAYER SENDER LEFT RIGHT))

(define-state-switch-function bidding-wait (player)
  "Wechelt in den Zustand bidding-wait.
Dies wird nicht nur zu Beginn der ganzen Reizprozedur aufgerufen!"
  (SLOT-MAKUNBOUND PLAYER 'BIDDING-MATE))

(defkernelmethod start-new-game (player)
  "Bereitet alles für eine neue Runde vor. Setzt die Reizwerte zurück,
vergisst, wer zuerst dran sein müsste, und wechselt nach Bidding-Wait."
  ;; Diese Zeilen dürfen nicht in die hierüberstehende Funktion,
  ;; da diese auch aufgerufen wird, nachdem der Spieler gepasst hat
  ;; oder ihm gepasst wurde
  (setf (table player) (cons nil (table player))) ; packe nil an den Tisch, als "keine Ahnung, wer dran ist"
  (call-ui 'game-start player (host player))	  ; UI Bescheid geben
  (setf (bidding-values player) (cut-away-game-point-levels 18)) ; Reizwerte zurücksetzen
  (switch-to-bidding-wait player))
  
(DEFHANDLER GAME-START (REGISTRATION-SUCCEEDED game-over) (ui host) (PLAYER)
  "Behandelt die Nachricht vom Host, dass die Runde beginnt und soll von
UI aufgerufen werden, wenn der Spieler die nächste Runde zu beginnen wünscht."
  (if (eq sender (ui player))
      (comm:send (comm player) (host player) 'game-start) ; von der UI
      (start-new-game player)))				  ; vom Host

(DEFHANDLER CARDS (BIDDING-wait) host (PLAYER CARDS)
   "Behandelt die Überreichung der Karten durch den Host."
   (SETF (CARDS PLAYER) CARDS)
   (CALL-UI 'CARDS PLAYER SENDER (cards player))) ; (cards player), da beim (setf) cards destruktiv sortiert wurde

(define-state-switch-function bid (player listener min-value)
  "Wechelt in den Zustand bid."
  (setf (bidding-mate player) listener)
  (call-ui 'start-bidding player (host player) listener min-value))

(DEFHANDLER START-BIDDING (BIDDING-wait) host (PLAYER LISTENER MIN-VALUE)
  "Behandelt die Anweisung vom Host, Reizwerte anzusagen."
  (if (null (car (table player)))	; bisher keine Ahnung, wer vorn ist
      ;; jetzt aber schon, da man als erster Sager Mittelhand ist
      ;; und Vorderhand sitzt rechts neben Mittelhand
      (turn-table-to player (right-playmate player)))
  (switch-to-bid player listener min-value))

(define-state-switch-function listen (player bidder)
  "Wechelt in den Zustand listen."
  (setf (bidding-mate player) bidder)
  (call-ui 'listen player (host player) bidder))

(DEFHANDLER LISTEN (BIDDING-wait) host (PLAYER BIDDER)
  "Behandelt die Anweisung vom Host, sich Reizwerte sagen zu lassen."
  (if (null (car (table player)))	; bisher keine Ahnung, wer vorn ist
      ;; jetzt aber schon, da man als erster Hörer selbst vorn sein wird
      (turn-table-to player (own-address player)))
  (switch-to-listen player bidder))

(DEFHANDLER BID (BIDDING-wait bid LISTEN) (ui left-playmate right-playmate) (PLAYER VALUE)
  "Behandelt einen angesagten Reizwert und soll von der UI aufgerufen werden,
wenn der Benutzer einen Spielwert reizt."
  (case-state player
    (bid 				; als Sager, also von der UI
     (with-correct-sender sender ((ui player))
       (send-to-all-others player 'bid value))) ; weiterschicken
    (bidding-wait			; als Dritter
     (if (null (car (table player)))	; bisher keine Ahnung wer vorn ist
	 ;; jetzt aber schon, da man als erster Unbeteiligter der Geber ist, also Hinterhand
	 (turn-table-to player (left-playmate player)))
     (CALL-UI 'BID PLAYER SENDER VALUE))
    (listen				; als Hörer
     (WITH-CORRECT-SENDER SENDER ((BIDDING-MATE PLAYER))
       (CALL-UI 'reply-to-BID PLAYER SENDER VALUE))))) ; Antwort von der UI verlangen

(DEFHANDLER JOIN (BIDDING-wait listen BID) (ui left-playmate right-playmate) (PLAYER VALUE)
  "Behandelt das Mitgehen des Hörers und soll von der UI aufgerufen werden, wenn der
Benutzer mitgeht."
  (case-state player
    (listen				; als Hörer, also von UI
     (with-correct-sender sender ((ui player))
       (send-to-all-others player 'join value))) ; weiterschicken
    (BIDDING-wait			; als Dritter
     (CALL-UI 'JOIN PLAYER SENDER VALUE))
    (bid				; als Sager
     (WITH-CORRECT-SENDER SENDER ((BIDDING-MATE PLAYER))
	 (CALL-UI 'join PLAYER SENDER VALUE)))))

(DEFHANDLER PASS (BIDDING-wait BID LISTEN) (ui left-playmate right-playmate) (PLAYER VALUE)
  "Behandelt das Passen des Sagers oder Hörers und soll von der UI aufgerufen werden,
wenn der Benutzer passen möchte."
  (case-state player
    (BIDDING-wait
     (when (null (car (table player)))	; bisher keine Ahnung wer vorn ist
       ;; wenn wir bis hierher kommen, dann muss der erste Sager sofort gepasst haben
       ;; sonst wäre dies schon beim BID oben erledigt worden
       ;; Der Spieler ist Geber. D. h. sein linker Mann spielt aus
       (turn-table-to player (left-playmate player)))
     (CALL-UI 'PASS PLAYER SENDER VALUE))
    (BID
     (if (equalp sender (ui player))	; selbst gepasst
	 (send-to-all-others player 'pass value) ; weiterschicken
	 (WITH-CORRECT-SENDER SENDER ((BIDDING-MATE PLAYER)) ; Hörer hat gepasst
	   (CALL-UI 'PASS PLAYER SENDER VALUE)))
     (SWITCH-to-bidding-wait PLAYER))
    (LISTEN
     (if (equalp sender (ui player))	; selbst gepasst
	 (send-to-all-others player 'pass value) ; weiterschicken
	 (WITH-CORRECT-SENDER SENDER ((BIDDING-MATE PLAYER))
	   (CALL-UI 'PASS PLAYER SENDER VALUE)))
     (switch-to-bidding-wait player))))

(define-state-switch-function preparations (player declarer)
  "Wechselt in den Zustand preparations."
  (SETF (DECLARER PLAYER) DECLARER)
  (CALL-UI 'DECLARER PLAYER (host player) DECLARER))

(DEFHANDLER DECLARER (BIDDING-wait bid) host (PLAYER DECLARER)
  "Behandelt die Bekanntgabe des Spielführers durch den Host."
  (when (eq (state player) 'bid)
    (assert (address-equal player (own-address player) declarer) ()
	    "~a hat DECLARER in Zustand BID empfangen, ist jedoch nicht selbst declarer!"
	    player))
  (SWITCH-to-PREPARATIONS player declarer))

(DEFHANDLER HAND-DECISION (PREPARATIONS) (ui declarer) (PLAYER HAND)
  "Behandelt die Handspielentscheidung des Spielführers und soll durch die
UI aufgerufen werden, wenn der Spieler sich entschieden hat, ob er den Skat
nehmen will."
  (if (equalp sender (ui player))
      (send-to-all-others PLAYER 'HAND-DECISION HAND) ; von der UI
      (call-ui 'hand-decision player sender hand)))   ; von draußen

(DEFHANDLER SKAT (PREPARATIONS) (host ui) (PLAYER SKAT)
  "Behandelt die Ausgabe des Skats durch den Host UND
soll durch die UI aufgerufen werden, wenn Karten in den Skat gedrückt werden."
  (if (equalp sender (ui player))	; von der UI
      (progn
	(DOLIST (CARD SKAT)	       ; gedrückte Karten aussortieren
	  (SETF (CARDS PLAYER)
		(DELETE CARD (CARDS PLAYER) :test #'equalp)))
	(SKAT-COMMUNICATION:SEND (COMM PLAYER) (HOST PLAYER)
				 'SKAT SKAT))
      (progn						   ; vom Host
	(SETF (CARDS PLAYER) (APPEND SKAT (CARDS PLAYER))) ; Skat zu Karten hinzufügen
	(CALL-UI 'SKAT PLAYER SENDER SKAT))))

(define-state-switch-function in-game (player declaration)
  "Wechelt in den Zustand in-game. Verschickt ggf. die Ansage."
  (assert (not (null (car (table player)))) ((table player))) ; Mittlerweile dürfte klar sein, wer vorn ist
  (setf (won-tricks player) nil	      ; gewonnene Stiche zurücksetzen
	(current-trick player) nil    ; aktuellen Stich initialisieren
	(game-declaration player) declaration)			    ; und sich die Ansage merken
  (if (address-equal player (own-address player) (declarer player)) ; selbst Spielführer?
      (SEND-TO-ALL-OTHERS PLAYER 'DECLARATION DECLARATION) ; Ansage verschicken
      (CALL-UI 'DECLARATION PLAYER (declarer player) DECLARATION))) ; UI Bescheid sagen

(define-state-switch-function play-cards (player))

(define-state-switch-function trick-full (player))

(DEFHANDLER DECLARATION (PREPARATIONS) (ui declarer) (PLAYER DECLARATION)
  "Behandelt die Ansage des Spielführers."
  (switch-to-in-game player declaration))

(DEFHANDLER CHOOSE-CARD (IN-GAME play-cards) host (PLAYER)
  "Behandelt die Mitteilung des Hosts, dass man am Stich ist."
  (if (eq (state player) 'in-game) (switch-to-play-cards player))
  (LOOP UNTIL (address-equal player (CAR (TABLE PLAYER)) (OWN-ADDRESS PLAYER))
     DO (TURN-TABLE PLAYER))		; Tisch zu sich selbst drehen
  (CALL-UI 'CHOOSE-CARD PLAYER SENDER))	; und UI in die Spur schicken

(define-condition suit-not-followed-error (error)
  ()
  (:report "Player did not follow suit of the first card"))

(defhandler card (in-game play-cards) (ui current-player) (player card)
  "Behandelt eine gespielte Karte und soll von der UI aufgerufen werden,
wenn der Benutzer eine Karte spielt."
  (if (eq (state player) 'in-game) (switch-to-play-cards player))
  (if (equalp sender (ui player))
      (progn
	(unless (address-equal player (current-player player) (own-address player))
	  ;; vor allem in der Debugzeit absichern, dass nur Spieler an der Reihe senden
	  (error "Spieler ist momentan nicht an der Reihe! Karte nicht gesendet!"))
	(unless (member card (cards player) :test #'equalp)
	  ;; absichern, dass Spieler nur Karten spielen, die sie auf der Hand haben
	  (error "Spieler besitzt diese Karte nicht! Karte nicht gesendet!"))
	(when (last (current-trick player))
	  ;; this card is just a supplement
	  (unless (same-suit-p (car (last (current-trick player)))
			       card
			       (car (game-declaration player)))
	    ;; player didn't follow suit
	    (when (find-if (rcurry (curry 'same-suit-p (car (last (current-trick player)))) (car (game-declaration player)))
			   (cards player))
	      ;; there are possible cards however, signal an error
	      (error 'suit-not-followed-error))))
	(setf (cards player) (delete card (cards player) :test #'equalp))
	(send-to-all-others player 'card card)) ; von UI
      ;; the card comes from somebody else
      (call-ui 'card player sender card))
  (push card (current-trick player))	     ; Karte für den aktuellen Stich eintragen
  (turn-table player)		       ; nächster Spieler
  (if (< (length (current-trick player)) 3)
      (when (address-equal player (own-address player) (current-player player))
	;; Spieler ist nun an der Reihe und der Stich ist noch nicht voll
	(call-ui 'choose-card player player)) ; Karte auswählen lassen
      ;; der Stich ist voll
      (switch-to-trick-full player)))

(DEFHANDLER TRICK (trick-full) host (PLAYER CARDS WINNER)
  "Behandelt die Auswertung des Stiches durch den Host."
  (IF (address-equal player winner (own-address player))
      (push CARDS (WON-TRICKS PLAYER)))	; zu den gewonnenen Stichen dazupacken
  (setf (current-trick player) nil)	; aktuellen Stich zurücksetzen
  (CALL-UI 'TRICK PLAYER SENDER CARDS WINNER) ; UI benachrichtigen
  (turn-table-to player winner)	       ; der Gewinner ist nun am Stich
  ;; und nun sollen wieder Karten gespielt werden
  (switch-to-play-cards player))

(define-state-switch-function game-over (player prompt &optional just-send-game-over)
  "Wechselt in den game-over Zustand."
  (declare (ignore just-send-game-over)) ; host hat auch ein switch-to-game-over
  (call-ui 'game-over player (host player) prompt))

(defhandler game-over (in-game play-cards bidding-wait game-over) host (player prompt)
  "Behandelt die Beendigung der Runde durch den Host."
  (switch-to-game-over player prompt))

(defhandler cards-score (game-over) host (player declarer-score defenders-score)
  "Behandelt die Punkteauszählung des Hostes."
  (call-ui 'cards-score player sender declarer-score defenders-score))

(defhandler game-result (game-over) host (player declaration won score)
  "Behandlet die Bekanntgabe des Rundenergebnisses durch den Host."
  (call-ui 'game-result player sender declaration won score))

(defhandler score-table (game-over) host (player player1-address player1-score
						 player2-address player2-score
						 player3-address player3-score)
  "Behandelt die Veröffentlichung der Punktetabelle durch den Host."
  (call-ui 'score-table player sender player1-address player1-score
	                              player2-address player2-score
				      player3-address player3-score))

(defhandler message () :any (player text)
  "Behandelt Textmitteilungen."
  (call-ui 'message player sender text))

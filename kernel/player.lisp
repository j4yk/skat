(in-package skat-kernel)

(defkernel player
    (start
     unregistered registration-pending registration-succeeded
     bidding-wait bid listen preparations
     in-game game-over)     
  ((ui :accessor ui :initarg :ui)
   (cards :accessor cards)
   (current-trick :accessor current-trick)
   (won-tricks :accessor won-tricks)
   (bidding-mate :accessor bidding-mate)
   (declarer :accessor declarer)
   (game-declaration :accessor game-declaration)
   (host :accessor host)
   (left-playmate :accessor left-playmate)
   (right-playmate :accessor right-playmate)
   (table :accessor table))
  (:documentation "Spielerobjekt. Hält alle Komponenten zusammen."))

(defmethod turn-table ((player player))
  "Bewegt die Ringliste in table einen Schritt weiter."
  (setf (table player) (cdr (table player))))

(defhandler login-parameters (start) comm (player parameters)
   "Behandelt die Loginparameterliste der Comm. Gibt sie an die UI weiter."
   (call-ui 'login-parameters player sender parameters))

(defhandler login-data (start) ui (player data)
  "Soll durch die UI aufgerufen werden.
Weist Comm an sich mit den Daten einzuloggen."
  (comm:login (comm player) data)
  (switch-state player 'unregistered))

(DEFHANDLER REGISTRATION-PARAMETERS (UNREGISTERED) comm (PLAYER PARAMETERS)
  "Behandelt die Parameterliste für die Registrierung von der Comm."
  (LET ((COMM SENDER))
    (CALL-UI 'REGISTRATION-PARAMETERS PLAYER COMM PARAMETERS)))

(DEFHANDLER REGISTRATION-DATA (UNREGISTERED) ui (PLAYER DATA)
  "Soll durch die UI aufgerufen werden.
Weist comm an sich mit den Daten bei einem Host zu registrieren."
  (SKAT-COMMUNICATION:REGISTER (COMM PLAYER) DATA)
  (SWITCH-STATE PLAYER 'REGISTRATION-PENDING))

(DEFHANDLER REGISTRATION-REPLY (REGISTRATION-PENDING) host (PLAYER ACCEPTED)
  "Behandelt die Antwort auf die Registrierungsanfrage vom Host."
  (IF ACCEPTED
      (PROGN
	(SETF (HOST PLAYER) SENDER)
	(SWITCH-STATE PLAYER 'REGISTRATION-SUCCEEDED))
      (SWITCH-STATE PLAYER 'UNREGISTERED))
  (CALL-UI 'LOGIN-PARAMETERS PLAYER SENDER ACCEPTED))

(DEFHANDLER SERVER-UPDATE (REGISTRATION-SUCCEEDED) host (PLAYER EVENTS)
  "Behandelt Neuigkeiten vom Host."
  (CALL-UI 'SERVER-UPDATE PLAYER SENDER EVENTS))

(DEFHANDLER UNREGISTER (REGISTRATION-SUCCEEDED) ui (PLAYER)
  "Soll von der UI aufgerufen werden, wenn der Spieler eine Loslösung
vom Host wünscht."
  (SKAT-COMMUNICATION:SEND (COMM PLAYER) (HOST PLAYER)
			   'UNREGISTER)
  (SLOT-MAKUNBOUND PLAYER 'HOST-ADDRESS)
  (SWITCH-STATE PLAYER 'UNREGISTERED))

(DEFHANDLER PLAYMATES (REGISTRATION-SUCCEEDED) host (PLAYER LEFT RIGHT)
  "Behandelt die Bekanntmachung der Mitspieler durch den Host."
  (SETF (LEFT-PLAYMATE PLAYER)
	LEFT
	(RIGHT-PLAYMATE PLAYER)
	RIGHT
	(TABLE PLAYER)
	(MAKE-RING (LIST (OWN-ADDRESS PLAYER) LEFT RIGHT)))
  (CALL-UI 'PLAYMATES PLAYER SENDER LEFT RIGHT))

(DEFHANDLER GAME-START (REGISTRATION-SUCCEEDED) host (PLAYER)
  "Behandelt die Nachricht vom Host, dass die Runde beginnt."
  (SWITCH-STATE PLAYER 'BIDDING)
  (CALL-UI 'GAME-START PLAYER SENDER))

(DEFHANDLER CARDS (BIDDING-wait) host (PLAYER CARDS)
   "Behandelt die Überreichung der Karten durch den Host."
   (SETF (CARDS PLAYER) CARDS)
   (CALL-UI CARDS PLAYER SENDER CARDS))

(DEFHANDLER START-BIDDING (BIDDING-wait) host (PLAYER LISTENER MIN-VALUE)
  "Behandelt die Anweisung vom Host, Reizwerte anzusagen."
  (SETF (BIDDING-MATE PLAYER) LISTENER) (SWITCH-STATE PLAYER 'BID)
  (CALL-UI 'START-BIDDING PLAYER SENDER LISTENER MIN-VALUE)
  (ERROR "Reizwerte fehlen noch."))

(DEFHANDLER LISTEN (BIDDING-wait) host (PLAYER BIDDER)
  "Behandelt die Anweisung vom Host, sich Reizwerte sagen zu lassen."
  (SETF (BIDDING-MATE PLAYER) BIDDER) (SWITCH-STATE PLAYER 'LISTEN)
  (CALL-UI 'LISTEN PLAYER SENDER BIDDER))

;; (defmacro with-correct-sender (sender correct-sender request-name &body body)
;;   "Führt body nur aus, wenn sender und correct-sender equal sind, andernfalls wird eine invalid-request-sender Condition signalisiert."
;;   `(if (equal ,sender ,correct-sender)
;;        (progn ,@body)
;;        (signal 'invalid-request-sender :sender ,sender :player-state (state player) :expected-sender ,correct-sender :request-name ',request-name)))

(DEFHANDLER BID (BIDDING-wait LISTEN) (left-playmate right-playmate) (PLAYER VALUE)
  "Behandelt einen angesagten Reizwert"
  (ECASE (STATE PLAYER)
    (BIDDING
     (CALL-UI 'BID PLAYER SENDER VALUE)
     (ERROR "TODO: Reizwerte!"))
    (LISTEN
     (WITH-CORRECT-SENDER SENDER ((BIDDING-MATE PLAYER))
	 (CALL-UI 'BID PLAYER SENDER VALUE)
       (ERROR "TODO: Reizwerte!")))))

(DEFHANDLER JOIN (BIDDING-wait BID) (left-playmate right-playmate) (PLAYER VALUE)
  "Behandelt das Mitgehen des Hörers."
  (ECASE (STATE PLAYER)
    (BIDDING (CALL-UI 'JOIN PLAYER SENDER VALUE))
    (LISTEN
     (WITH-CORRECT-SENDER SENDER ((BIDDING-MATE PLAYER))
	 (CALL-UI 'JOIN PLAYER SENDER VALUE)))))

(DEFHANDLER PASS (BIDDING-wait BID LISTEN) (left-playmate right-playmate) (PLAYER VALUE)
  "Behandelt das Passen des Sagers oder Hörers."
  (ECASE (STATE PLAYER)
    (BIDDING (CALL-UI 'PASS PLAYER SENDER VALUE))
    (BID
     (WITH-CORRECT-SENDER SENDER ((BIDDING-MATE PLAYER))
	 (CALL-UI 'PASS PLAYER SENDER VALUE)
       (SLOT-MAKUNBOUND PLAYER 'BIDDING-MATE)
       (SWITCH-STATE PLAYER 'BIDDING)))
    (LISTEN
     (WITH-CORRECT-SENDER SENDER ((BIDDING-MATE PLAYER))
	 (CALL-UI 'PASS PLAYER SENDER VALUE)
       (SLOT-MAKUNBOUND PLAYER 'BIDDING-MATE)
       (SWITCH-STATE PLAYER 'BIDDING)))))

(DEFHANDLER DECLARER (BIDDING-wait) host (PLAYER DECLARER)
   "Behandelt die Bekanntgabe des Spielführers durch den Host."
   (SWITCH-STATE 'PREPARATIONS)
   (SETF (DECLARER PLAYER) DECLARER)
   (CALL-UI 'DECLARER PLAYER SENDER DECLARER))

(defmethod send-to-all-others ((player player) request-name &rest request-args)
  "Ruft comm:send mit gleicher Anfrage für Host, linken und rechten Mitspieler auf."
  (dolist (receiver (list (host player) (left-playmate player) (right-playmate player)))
    (apply #'comm:send (comm player) receiver request-name request-args)))

(DEFHANDLER HAND-DECISION (PREPARATIONS) declarer (PLAYER HAND)
  "Behandelt die Handspielentscheidung des Spielführers."
  (IF (EQUAL SENDER (OWN-ADDRESS PLAYER))
      (SEND-TO-ALL-OTHERS PLAYER 'HAND-DECISION HAND)
      (WITH-CORRECT-SENDER SENDER ((DECLARER PLAYER))
	  (CALL-UI 'HAND-DECISION PLAYER SENDER HAND))))

(DEFHANDLER SKAT (PREPARATIONS) (host ui) (PLAYER SKAT)
  "Behandelt die Ausgabe des Skats durch den Host UND
soll durch die UI aufgerufen werden, wenn Karten in den Skat gedrückt werden."
  (COND
    ((EQUAL SENDER (HOST PLAYER))
     (SETF (CARDS PLAYER) (APPEND SKAT (CARDS PLAYER)))
     (CALL-UI 'SKAT PLAYER SENDER SKAT))
    ((EQUAL SENDER (OWN-ADDRESS PLAYER))
     (DOLIST (CARD SKAT)
       (SETF (CARDS PLAYER)
	     (DELETE CARD (CARDS PLAYER) :KEY #'EQUAL)))
     (SKAT-COMMUNICATION:SEND (COMM PLAYER) (HOST PLAYER)
			      'SKAT SKAT))
    (T
     (SIGNAL 'INVALID-REQUEST-SENDER :SENDER SENDER :EXPECTED-SENDER
	     (BIDDING-MATE PLAYER) :PLAYER-STATE (STATE PLAYER)
	     :REQUEST-NAME 'JOIN))))

(DEFHANDLER DECLARATION (PREPARATIONS) declarer (PLAYER DECLARATION)
  "Behandelt die Ansage des Spielführers."
  (WITH-CORRECT-SENDER SENDER ((DECLARER PLAYER))
      (SETF (GAME-DECLARATION PLAYER) DECLARATION)
    (IF (EQUAL SENDER (OWN-ADDRESS PLAYER))
	(SEND-TO-ALL-OTHERS PLAYER 'DECLARATION DECLARATION)
	(CALL-UI 'DECLARATION PLAYER SENDER DECLARATION))
    (SWITCH-STATE 'IN-GAME)))

(DEFHANDLER CHOOSE-CARD (IN-GAME) host (PLAYER)
  "Behandelt die Mitteilung des Hosts, dass man am Stich ist."
  (WITH-CORRECT-SENDER SENDER ((HOST PLAYER))
      (LOOP UNTIL
	   (EQUAL (CAR (TABLE PLAYER)) (OWN-ADDRESS PLAYER)) DO
	   (TURN-TABLE PLAYER))
    (CALL-UI 'CHOOSE-CARD PLAYER SENDER)))

;(defhandler card ;; ... UI->Komm, richter-Spieler->UI, turn table nicht vergessen

(defhandler card (in-game) (left-playmate right-playmate) (player card)
  "Behandelt eine gespielte Karte."
  (call-ui 'card player sender card))

(DEFHANDLER TRICK (IN-GAME) host (PLAYER CARDS WINNER)
  "Behandelt die Auswertung des Stiches durch den Host."
  (WITH-CORRECT-SENDER SENDER ((HOST PLAYER))
      (IF (EQUAL WINNER (OWN-ADDRESS PLAYER))
	  (CONS CARDS (WON-TRICKS PLAYER)))
    (CALL-UI 'TRICK PLAYER SENDER CARDS WINNER)))

(defhandler game-over (in-game) host (player prompt)
  "Behandlet die Beendigung der Runde durch den Host."
  (call-ui 'game-over player sender prompt))

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

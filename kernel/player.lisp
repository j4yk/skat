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
   (host-address :accessor host-address)
   (left-playmate :accessor left-playmate)
   (right-playmate :accessor right-playmate)
   (table :accessor table))
  (:documentation "Spielerobjekt. Hält alle Komponenten zusammen."))

(defmethod turn-table ((player player))
  "Bewegt die Ringliste in table einen Schritt weiter."
  (setf (table player) (cdr (table player))))

(defhandler login-parameters (start) (player parameters)
  (let ((comm sender))
    (call-ui 'login-parameters player comm parameters)))

(defhandler login-data (start) (player data)
  (comm:login (comm player) data)
  (switch-state player 'unregistered))

(DEFHANDLER REGISTRATION-PARAMETERS (UNREGISTERED) (PLAYER PARAMETERS)
  (LET ((COMM SENDER))
    (CALL-UI 'REGISTRATION-PARAMETERS PLAYER COMM PARAMETERS)))

(DEFHANDLER REGISTRATION-DATA (UNREGISTERED) (PLAYER DATA)
  (SKAT-COMMUNICATION:REGISTER (COMM PLAYER) DATA)
  (SWITCH-STATE PLAYER 'REGISTRATION-PENDING))

(DEFHANDLER REGISTRATION-REPLY (REGISTRATION-PENDING) (PLAYER ACCEPTED)
  (IF ACCEPTED
      (PROGN
	(SETF (HOST-ADDRESS PLAYER) SENDER)
	(SWITCH-STATE PLAYER 'REGISTRATION-SUCCEEDED))
      (SWITCH-STATE PLAYER 'UNREGISTERED))
  (CALL-UI 'LOGIN-PARAMETERS PLAYER SENDER ACCEPTED))

(DEFHANDLER SERVER-UPDATE (REGISTRATION-SUCCEEDED) (PLAYER EVENTS)
  (CALL-UI 'SERVER-UPDATE PLAYER SENDER EVENTS))

(DEFHANDLER UNREGISTER (REGISTRATION-SUCCEEDED) (PLAYER)
  (SKAT-COMMUNICATION:SEND (COMM PLAYER) (HOST-ADDRESS PLAYER)
			   'UNREGISTER)
  (SLOT-MAKUNBOUND PLAYER 'HOST-ADDRESS)
  (SWITCH-STATE PLAYER 'UNREGISTERED))

(DEFHANDLER PLAYMATES (REGISTRATION-SUCCEEDED) (PLAYER LEFT RIGHT)
  (SETF (LEFT-PLAYMATE PLAYER)
	LEFT
	(RIGHT-PLAYMATE PLAYER)
	RIGHT
	(TABLE PLAYER)
	(MAKE-RING (LIST (OWN-ADDRESS PLAYER) LEFT RIGHT)))
  (CALL-UI 'PLAYMATES PLAYER SENDER LEFT RIGHT))

(DEFHANDLER GAME-START (REGISTRATION-SUCCEEDED) (PLAYER)
  (SWITCH-STATE PLAYER 'BIDDING)
  (CALL-UI 'GAME-START PLAYER SENDER))

(DEFHANDLER CARDS (BIDDING) (PLAYER CARDS) (SETF (CARDS PLAYER) CARDS)
            (CALL-UI CARDS PLAYER SENDER CARDS))

(DEFHANDLER START-BIDDING (BIDDING) (PLAYER LISTENER MIN-VALUE)
  (SETF (BIDDING-MATE PLAYER) LISTENER) (SWITCH-STATE PLAYER 'BID)
  (CALL-UI 'START-BIDDING PLAYER SENDER LISTENER MIN-VALUE)
  (ERROR "Reizwerte fehlen noch."))

(DEFHANDLER LISTEN (BIDDING) (PLAYER BIDDER)
  (SETF (BIDDING-MATE PLAYER) BIDDER) (SWITCH-STATE PLAYER 'LISTEN)
  (CALL-UI 'LISTEN PLAYER SENDER BIDDER))

;; (defmacro with-correct-sender (sender correct-sender request-name &body body)
;;   "Führt body nur aus, wenn sender und correct-sender equal sind, andernfalls wird eine invalid-request-sender Condition signalisiert."
;;   `(if (equal ,sender ,correct-sender)
;;        (progn ,@body)
;;        (signal 'invalid-request-sender :sender ,sender :player-state (state player) :expected-sender ,correct-sender :request-name ',request-name)))

(DEFHANDLER BID (BIDDING LISTEN) (PLAYER VALUE)
  (ECASE (STATE PLAYER)
    (BIDDING
     (CALL-UI 'BID PLAYER SENDER VALUE)
     (ERROR "TODO: Reizwerte!"))
    (LISTEN
     (WITH-CORRECT-SENDER SENDER ((BIDDING-MATE PLAYER))
	 (CALL-UI 'BID PLAYER SENDER VALUE)
       (ERROR "TODO: Reizwerte!")))))

(DEFHANDLER JOIN (BIDDING BID) (PLAYER VALUE)
  (ECASE (STATE PLAYER)
    (BIDDING (CALL-UI 'JOIN PLAYER SENDER VALUE))
    (LISTEN
     (WITH-CORRECT-SENDER SENDER ((BIDDING-MATE PLAYER))
	 (CALL-UI 'JOIN PLAYER SENDER VALUE)))))

(DEFHANDLER PASS (BIDDING BID LISTEN) (PLAYER VALUE)
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

(DEFHANDLER DECLARER (BIDDING) (PLAYER DECLARER) (SWITCH-STATE 'PREPARATIONS)
            (SETF (DECLARER PLAYER) DECLARER)
            (CALL-UI 'DECLARER PLAYER SENDER DECLARER))

(defmethod send-to-all-others ((player player) request-name &rest request-args)
  "Ruft comm:send mit gleicher Anfrage für Host, linken und rechten Mitspieler auf."
  (dolist (receiver (list (host-address player) (left-playmate player) (right-playmate player)))
    (apply #'comm:send (comm player) receiver request-name request-args)))

(DEFHANDLER HAND-DECISION (PREPARATIONS) (PLAYER HAND)
  (IF (EQUAL SENDER (OWN-ADDRESS PLAYER))
      (SEND-TO-ALL-OTHERS PLAYER 'HAND-DECISION HAND)
      (WITH-CORRECT-SENDER SENDER ((DECLARER PLAYER))
	  (CALL-UI 'HAND-DECISION PLAYER SENDER HAND))))

(DEFHANDLER SKAT (PREPARATIONS) (PLAYER SKAT)
  (COND
    ((EQUAL SENDER (HOST-ADDRESS PLAYER))
     (SETF (CARDS PLAYER) (APPEND SKAT (CARDS PLAYER)))
     (CALL-UI 'SKAT PLAYER SENDER SKAT))
    ((EQUAL SENDER (OWN-ADDRESS PLAYER))
     (DOLIST (CARD SKAT)
       (SETF (CARDS PLAYER)
	     (DELETE CARD (CARDS PLAYER) :KEY #'EQUAL)))
     (SKAT-COMMUNICATION:SEND (COMM PLAYER) (HOST-ADDRESS PLAYER)
			      'SKAT SKAT))
    (T
     (SIGNAL 'INVALID-REQUEST-SENDER :SENDER SENDER :EXPECTED-SENDER
	     (BIDDING-MATE PLAYER) :PLAYER-STATE (STATE PLAYER)
	     :REQUEST-NAME 'JOIN))))

(DEFHANDLER DECLARATION (PREPARATIONS) (PLAYER DECLARATION)
  (WITH-CORRECT-SENDER SENDER ((DECLARER PLAYER))
      (SETF (GAME-DECLARATION PLAYER) DECLARATION)
    (IF (EQUAL SENDER (OWN-ADDRESS PLAYER))
	(SEND-TO-ALL-OTHERS PLAYER 'DECLARATION DECLARATION)
	(CALL-UI 'DECLARATION PLAYER SENDER DECLARATION))
    (SWITCH-STATE 'IN-GAME)))

(DEFHANDLER CHOOSE-CARD (IN-GAME) (PLAYER)
  (WITH-CORRECT-SENDER SENDER ((HOST-ADDRESS PLAYER))
      (LOOP UNTIL
	   (EQUAL (CAR (TABLE PLAYER)) (OWN-ADDRESS PLAYER)) DO
	   (TURN-TABLE PLAYER))
    (CALL-UI 'CHOOSE-CARD PLAYER SENDER)))

;(defhandler card ;; ... UI->Komm, richter-Spieler->UI, turn table nicht vergessen

(DEFHANDLER TRICK (IN-GAME) (PLAYER CARDS WINNER)
  (WITH-CORRECT-SENDER SENDER ((HOST-ADDRESS PLAYER))
      (IF (EQUAL WINNER (OWN-ADDRESS PLAYER))
	  (CONS CARDS (WON-TRICKS PLAYER)))
    (CALL-UI 'TRICK PLAYER SENDER CARDS WINNER)))


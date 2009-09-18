(in-package skat-kernel)

(defun make-ring (proper-list)
  "Wandelt eine Kopie der normalen Liste in eine geschlossene Ringliste um.
Nebeneffekte: setzt *print-circle* auf t."
  (setq *print-circle* t)
  (let* ((list (copy-list proper-list)))
    (setf (cdr (last list)) list) ; Ringschluss
    list))

(defclass player (base-kernel)
  ((ui :accessor ui :initarg :ui)
   (comm :accessor comm :initarg :comm)
   (cards :accessor cards)
   (current-trick :accessor current-trick)
   (won-tricks :accessor won-tricks)
   (bidding-mate :accessor bidding-mate)
   (declarer :accessor declarer)
   (game-declaration :accessor game-declaration)
   (state :accessor state)
   (own-address :accessor own-address)
   (host-address :accessor host-address)
   (left-playmate :accessor left-playmate)
   (right-playmate :accessor right-playmate)
   (table :accessor table)))

(defmethod turn-table ((player player))
  "Bewegt die Ringliste in table einen Schritt weiter."
  (setf (table player) (cdr (table player))))

(defmacro call-ui (request-name player sender &rest request-args)
  "Stellt die Anfrage an die UI weiter."
  `(apply #',(intern (symbol-name (handler-fn-name request-name)) 'skat-ui) (ui ,player) ,sender ,@request-args))

(defhandler login-parameters (start) (player comm parameters)
  (call-ui login-parameters player comm parameters))

(defhandler login-data (start) (player ui data)
  (comm:login (comm player) data)
  (switch-state player 'unregistered))

(defhandler registration-parameters (unregistered) (player comm parameters)
  (call-ui registration-parameters player comm parameters))

(defhandler registration-data (unregistered) (player ui data)
  (comm:register (comm player) data)
  (switch-state player 'registration-pending))

(defhandler registration-reply (registration-pending) (player sender accepted)
  (if accepted
      (progn
	(setf (host-address player) sender)
	(switch-state player 'registration-succeeded))
      (switch-state player 'unregistered))
  (call-ui login-parameters player sender accepted))

(defhandler server-update (registration-succeeded) (player sender events)
  (call-ui server-update player sender events))

(defhandler unregister (registration-succeeded) (player ui)
  (comm:send (comm player) (host-address player) 'unregister) 
  (slot-makunbound player 'host-address)
  (switch-state player 'unregistered))

(defhandler playmates (registration-succeeded) (player sender left right)
  (setf (left-playmate player) left
	(right-playmate player) right
	(table player) (make-ring (list (own-address player) left right)))
  (call-ui playmates player sender left right))

(defhandler game-start (registration-succeeded) (player sender)
  (switch-state player 'bidding)
  (call-ui game-start player sender))

(defhandler cards (bidding) (player sender cards)
  (setf (cards player) cards)
  (call-ui cards player sender cards))

(defhandler start-bidding (bidding) (player sender listener min-value)
  (setf (bidding-mate player) listener)
  (switch-state player 'bid)
  (call-ui start-bidding player sender listener min-value)
  (error "Reizwerte fehlen noch."))

(defhandler listen (bidding) (player sender bidder)
  (setf (bidding-mate player) bidder)
  (switch-state player 'listen)
  (call-ui listen player sender bidder))

(define-condition invalid-request-sender (error)
  ((sender :accessor sender :initarg :sender)
   (player-state :accessor player-state :initarg :player-state)
   (expected-sender :accessor expected-sender :initarg :expected-sender)
   (request-name :accessor request-name :initarg :request-name)))

(defmacro with-correct-sender (sender correct-sender request-name &body body)
  "Führt body nur aus, wenn sender und correct-sender equal sind, andernfalls wird eine invalid-request-sender Condition signalisiert."
  `(if (equal ,sender ,correct-sender)
       (progn ,@body)
       (signal 'invalid-request-sender :sender ,sender :player-state (state player) :expected-sender ,correct-sender :request-name ',request-name)))

(defhandler bid (bidding listen) (player sender value)
  (ecase (state player)
    (bidding 
     (call-ui bid player sender value)
     (error "TODO: Reizwerte!"))
    (listen 
     (with-correct-sender sender (bidding-mate player) bid
       (call-ui bid player sender value)
       (error "TODO: Reizwerte!")))))

(defhandler join (bidding bid) (player sender value)
  (ecase (state player)
    (bidding
     (call-ui join player sender value))
    (listen
     (with-correct-sender sender (bidding-mate player) join
       (call-ui join player sender value)))))

(defhandler pass (bidding bid listen) (player sender value)
  (ecase (state player)
    (bidding
     (call-ui pass player sender value))
    (bid
     (with-correct-sender sender (bidding-mate player) pass
       (call-ui pass player sender value)
       (slot-makunbound player 'bidding-mate)
       (switch-state player 'bidding)))
    (listen
     (with-correct-sender sender (bidding-mate player) pass
       (call-ui pass player sender value)
       (slot-makunbound player 'bidding-mate)
       (switch-state player 'bidding)))))

(defhandler declarer (bidding) (player sender declarer)
  (switch-state 'preparations)
  (setf (declarer player) declarer)
  (call-ui declarer player sender declarer))

(defmethod send-to-all-others ((player player) request-name &rest request-args)
  "Ruft comm:send mit gleicher Anfrage für Host, linken und rechten Mitspieler auf."
  (dolist (receiver (list (host-address player) (left-playmate player) (right-playmate player)))
    (apply #'comm:send (comm player) receiver request-name request-args)))

(defhandler hand-decision (preparations) (player sender hand)
  (if (equal sender (own-address player))
      (send-to-all-others player 'hand-decision hand)
      (with-correct-sender sender (declarer player) hand-decision
	(call-ui hand-decision player sender hand))))

(defhandler skat (preparations) (player sender skat)
  (cond ((equal sender (host-address player))
	 ;; Skat vom Host
	 (setf (cards player) (append skat (cards player)))
	 (call-ui skat player sender skat))
	((equal sender (own-address player))
	 ;; gedrückte Karten von UI
	 (dolist (card skat)
	   (setf (cards player) (delete card (cards player) :key #'equal)))
	 (comm:send (comm player) (host-address player) 'skat skat))
	(t (signal 'invalid-request-sender :sender sender :expected-sender (bidding-mate player) :player-state (state player) :request-name 'join))))

(defhandler declaration (preparations) (player sender declaration)
  (with-correct-sender sender (declarer player) declaration
    (setf (game-declaration player) declaration)
    (if (equal sender (own-address player))
	(send-to-all-others player 'declaration declaration)
	(call-ui declaration player sender declaration))
    (switch-state 'in-game)))

(defhandler choose-card (in-game) (player sender)
  (with-correct-sender sender (host-address player) choose-card
    (loop until (equal (car (table player)) (own-address player)) do (turn-table player))
    (call-ui choose-card player sender)))

;(defhandler card ;; ... UI->Komm, richter-Spieler->UI, turn table nicht vergessen

(defhandler trick (in-game) (player sender cards winner)
  (with-correct-sender sender (host-address player) trick
    (if (equal winner (own-address player))
	(cons cards (won-tricks player)))
    (call-ui trick player sender cards winner)))


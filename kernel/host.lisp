(in-package skat-kernel)

(defclass host (base-kernel)
  ((registered-players :accessor registered-players :initform nil)
   (dealers :accessor dealers :initform nil :documentation "ringlist of players, dealer is car")
   (current-declarer :accessor current-declarer)
   (score-table :accessor score-table)
   (skat :accessor skat :documentation "Noch nicht ausgegebener Skat")
   (want-game-start :accessor want-game-start :initform nil 
		    :documentation "Liste der Spieler, die eine neue Runde wollen")))

(defhandler login-parameters (start) (host comm parameters)
  (call-ui login-parameters host comm parameters))

(defhandler login-data (start) (host ui data)
  (comm:login (comm host) data)
  (switch-state host 'registration))

(defmethod send-to-players ((host host) request-name &rest request-args)
  "Sendet einen request an alle registrierten Spieler."
  (dolist (receiver (registered-players host))
    (apply #'comm:send (comm host) receiver request-name request-args)))

(defun shuffle (list)
  (if (null list)
      nil
      (let ((card (nth (random (length list)) list)))
	(cons card (shuffle (remove card list :count 1))))))

(defun shuffle-test ()
  (let* ((list (loop for i from 0 to 10 collect (random 10)))
	 (shuffled-list (shuffle list)))
    (format *debug-io* "~&function shuffle: ~s => ~s" list shuffled-list)
    (assert (= (length list) (length shuffled-list)))
    (dolist (item list)
      (assert (member item shuffled-list))
      (assert (= (count item shuffled-list) (count item list))))))

(defmethod current-dealer ((host host))
  (first (dealers host)))

(defmethod current-forehand ((host host))
  (second (dealers host)))

(defmethod current-middlehand ((host host))
  (third (dealers host)))

(defmethod start-game ((host host))
  "Starts the actual game after enough hosts have registered"
  (assert (= (length (registered-players host)) 3) ((registered-players host)))
  (switch-state host 'bidding-1)
  (send-to-players host 'game-start)
  (let ((cards (shuffle (all-cards))))
    (setf (skat host) (subseq cards 0 2))
    (comm:send (comm host) (first (registered-players host)) cards (subseq cards 2 12))
    (comm:send (comm host) (second (registered-players host)) cards (subseq cards 12 22))
    (comm:send (comm host) (third (registered-players host)) cards (subseq cards 22 32))
    (turn-ring (dealers host))
    (comm:send (comm host) (current-forehand host) 'listen (current-middlehand host))
    (comm:send (comm host) (current-middlehand host) 'start-bidding (current-forehand host))
    ;; und jetzt warte auf pass
    ))

(defhandler registration-request (registration) (host sender)
  ;; FIXME: muss in allen States akzeptiert werden (zum Ablehnen)
  (case (state host)
    (registration
       (if (>= (length (registered-players host)) 3)
	   (comm:send (comm host) sender 'registration-reply nil) ; es gibt schon drei Spieler
	   (unless (member sender (registered-players host) :test (address-compare-function host))
	     (send-to-players host 'server-update `(:player-join ,sender))
	     (push sender (registered-players host)) ; Spieler aufnehmen
	     (comm:send (comm host) sender 'registration-reply t)
	     (if (= (length (registered-players host)) 3)
		 (setf (dealers host) (make-ring (registered-players host))) ; setze die Spieler an einen runden Tisch
		 (start-game host)))))
    (otherwise
     (comm:send (comm host) sender 'registration-reply nil)))) ; es werden keine Registrierungen akzeptiert

(defhandler game-start (game-over) (host sender)
  (unless (member sender (want-game-start host) :test (address-compare-function host))
    (push sender (want-game-start host)))
  (if (null (set-difference (want-game-start host) (registered-players host)))
      (switch-state host 'bidding-1)))

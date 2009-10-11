(in-package skat-kernel)

(defkernel host (start registration
		       bidding-1 bidding-2 bidding-3
		       declarer-found skat-away await-declaration
		       in-game game-over)
  ((registered-players :accessor registered-players :initform nil)
   (dealers :accessor dealers :documentation "ringlist of players, dealer is car")
   (current-declarer :accessor current-declarer)
   (score-table :accessor score-table)
   (skat :accessor skat :documentation "Noch nicht ausgegebener Skat")
   (want-game-start :accessor want-game-start :initform nil 
		    :documentation "Liste der Spieler, die eine neue Runde wollen")))

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

;; state: start. Alles vor dem Registrieren.

(defhandler login-parameters (start) (host comm parameters)
  "Von der Kommunikation kommende Parameter zum Einwählen ins Kommunikationsmedium"
  (call-ui login-parameters host comm parameters))

(defhandler login-data (start) (host ui data)
  (comm:login (comm host) data)
  (switch-state host 'registration))

;; state: registration, alles

(defhandler registration-request () (host sender)
  "Behandelt Anfragen von Spielern, ob sie sich an den Tisch setzen dürfen"
  (case (state host)
    (registration
     ;; während der Registrierungsphase werden Registrierungen akzeptiert
     (if (>= (length (registered-players host)) 3)
	 (comm:send (comm host) sender 'registration-reply nil) ; es gibt schon drei Spieler
	 (unless (member sender (registered-players host) :test (address-compare-function host))
	   (send-to-players host 'server-update `(:player-join ,sender))
	   (push sender (registered-players host)) ; Spieler aufnehmen
	   (comm:send (comm host) sender 'registration-reply t)
	   (when (= (length (registered-players host)) 3)
	     (setf (dealers host) (make-ring (registered-players host))) ; setze die Spieler an einen runden Tisch
	     ;;(start-game host)))))  	  ; erstmal warten, bis alle game-start gesendet haben
	     ))))
    (otherwise
     ;; es werden keine Registrierungen akzeptiert
     (comm:send (comm host) sender 'registration-reply nil)
     (comm:send (comm host) sender 'message "Host is not in registration mode."))))

(defhandler unregister () (host sender)
  "Behandelt die Nachricht eines Spielers, dass er die Runde verlässt."
  (ecase (state host)
    (registration
     ;; Spieler aus der Liste entfernen
     (setf (registered-players host)
	   (delete sender (registered-players host) :test (address-compare-function host)))
     (slot-makunbound host 'dealers)	  ; die Tischrunde auflösen
     (inform-players-of-leaving-player)
     (send-to-players host 'message (format nil "Spieler ~a verlässt die Runde." sender)))))

(defhandler game-start (game-over) (host sender)
  (unless (member sender (want-game-start host) :test (address-compare-function host))
    (push sender (want-game-start host))) ; vermerke, dass der Spieler game-start gesendet hat
  (if (null (set-difference (want-game-start host) (registered-players host)))
      (switch-state host 'bidding-1)))


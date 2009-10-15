(in-package skat-kernel)

(defkernel host (start registration
		       bidding-1 bidding-2 bidding-3
		       declarer-found skat-away await-declaration
		       in-game game-over)
  ((registered-players :accessor registered-players :initform nil)
   (dealers :accessor dealers :documentation "ringlist of players, dealer is car")
   (bidding-values :accessor bidding-values :documentation "verbliebene mögliche Reizwerte")
   (current-declarer :accessor current-declarer :documentation "Adresse des aktuellen Spielführers")
   (score-table :accessor score-table :documentation "die Punktetabelle aus (cons Adresse Punktzahl)")
   (skat :accessor skat :documentation "Noch nicht ausgegebener Skat")
   (tricks :accessor tricks :documentation "Gespielte Stiche")
   (want-game-start :accessor want-game-start :initform nil 
		    :documentation "Liste der Spieler, die eine neue Runde wollen")))

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

(defun shuffle-test ()
  "Testcase für #'shuffle"
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

(defmethod reset-bidding-values ((host host))
  "Setzt die verbliebenen Reizwerte auf den Anfangszustand (also alle Werte ab 18) zurück."
  (setf (bidding-values host) (cut-away-bidding-values 18)))

;; state: start. Alles vor dem Registrieren.

(defhandler login-parameters (start) (host parameters)
  "Von der Kommunikation kommende Parameter zum Einwählen ins Kommunikationsmedium"
  (let ((comm sender))
    (call-ui login-parameters host comm parameters)))

(defhandler login-data (start) (host data)
  (let ((ui sender))
    (comm:login (comm host) data)
    (switch-state host 'registration)))

;; state: registration, alles

(defhandler registration-request () (host)
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

(defhandler unregister () (host)
  "Behandelt die Nachricht eines Spielers, dass er die Runde verlässt."
  (ecase (state host)
    (registration
     ;; Spieler aus der Liste entfernen
     (setf (registered-players host)
	   (delete sender (registered-players host) :test (address-compare-function host)))
     (slot-makunbound host 'dealers)	  ; die Tischrunde auflösen
     (inform-players-of-leaving-player)
     (send-to-players host 'message (format nil "Spieler ~a verlässt die Runde." sender)))))

;; game-start wird ansonsten vorrangig im game-over state behandelt
;; deshalb steht der Handler unten

;; state: bidding-1. Erste Stufe des Reizens
;;        bidding-2. Zweite Reizrunde
;;        bidding-3. Dritte Reizrunde (Ramschentscheidung)

(defmacro listen-to (listener bidder)
  "Sendet zwei Spielern die Befehle zum gegenseitigen Reizen."
  `(progn
     (comm:send (comm host) ,listener 'listen ,bidder)
     (comm:send (comm host) ,bidder 'start-bidding ,listener)))

(define-state-entering-function bidding-1 host
  "Startet das eigentliche Spiel, teilt die Karten aus und startet den Reizvorgang."
  (with-slots (registered-players skat comm dealers bidding-values) host
    (assert (= (length registered-players) 3) (registered-players))
    (switch-state host 'bidding-1)
    (send-to-players host 'game-start)
    (let ((cards (shuffle (all-cards))))
      (setf skat (subseq cards 0 2))
      (comm:send comm (first registered-players) cards (subseq cards 2 12))
      (comm:send comm (second registered-players) cards (subseq cards 12 22))
      (comm:send comm (third registered-players) cards (subseq cards 22 32)))
    (setf dealers (cdr dealers))
    (symbol-macrolet ((current-forehand (current-forehand host))
		      (current-middlehand (current-middlehand host)))
      (listen-to current-forehand current-middlehand))
    (reset-bidding-values host)
    ;; und jetzt warte auf pass
    ))

(defhandler bid (bidding-1 bidding-2 bidding-3) (host value)
  "Behandelt das Ansagen eines Reizwertes durch einen Spieler."
  (setf (bidding-values host) (cut-away-bidding-values value (bidding-values host))) ; Reizwerte aktualisieren
  )

(defhandler join (bidding-1 bidding-2 bidding-3) (host value)
  "Behandelt das Mitgehen eines Spielers bei einem Reizwert."
  (case (state host)
    (bidding-1
     ;; in der ersten Reizrunde geht einer mit, d. h. Reizen ist schonmal unmöglich
     (setf (current-declarer sender) sender))))

(defmacro player-case (player &body cases)
  "(case-player {address}
  (current-dealer {form}*)?
  (current-forehand {form}*)?
  (current-middlehand {form}*)?)"
  `(cond
     ,@(loop for case in cases
	  collect `((funcall (address-compare-function host) ,player (,(car case) host))
		    ,@(cdr case)))
     (t (error 'invalid-sender))))
     

(defhandler pass (bidding-1 bidding-2 bidding-3) (host value)
  "Behandelt das Passen eines Mitspielers bei einem Reizwert."
  (with-correct-sender sender ((current-listener host) (current-bidder host))
    ;; sender passed
    (ecase (state host)
      (bidding-1			; erster Pass
       (player-case sender
	 (current-forehand 		; Vorderhand hat gepasst
	  ;; Geber sagt Mittelhand weiter, Mittelhand hat einen Reizwert gesagt
	  (bidding-2 (current-middlehand host) (current-dealer host)
		     :declarer (current-middlehand host)))
	 (current-middlehand		; Mittelhand hat gepasst
	  ;; Geber sagt Vorderhand weiter, keine Information über Reizwerte
	  (bidding-2 (current-forehand host) (current-dealer host)))))
      (bidding-2			; zweiter Pass
       (player-case sender
	 (current-forehand		; Vorderhand hat gepasst
	  ;; d. h. Geber spielt
	  (declarer-found (current-dealer host)))
	 (current-middlehand		; Mittelhand hat gepasst
	  ;; d. h. Geber spielt
	  (declarer-found (current-dealer host)))
	 (current-dealer		; Geber hat gepasst
	  (if (null (current-declarer host))
	      ;; noch hat keiner etwas gereizt, d. h. Vorderhand entscheidet über Ramsch
	      (bidding-3)
	      ;; es hat schon jemand etwas gereizt und nicht gepasst, derjenige spielt
	      (declarer-found (current-declarer host))))))
      (bidding-3			; dritter Pass => Ramsch
       (ramschen)))))

;; state: declarer-found. Warte auf hand-decision.

(defhandler hand-decision (declarer-found) (host hand)
  "Behandelt die Ansage, ob der Declarer Hand spielt."
  )

;; state: skat-away. Warte auf Rückgabe des Skats.

(defhandler skat (skat-away) (host  skat)
  "Behandelt die Rückgabe des Skats vom Declarer."
  )

;; state: await-declaration. Warte auf die Ansage des Declarers.

(defhandler declaration (await-declaration) (host declaration)
  "Behandelt die Verkündung der Ansage des Declarers."
  )

;; state: in-game. Das Spiel läuft, die Stiche werden mitgenommen

(defhandler card (in-game) (host card)
  "Behandelt das Spielen einer Karte durch einen Spieler."
  )

;; state: game-over. Das Spiel ist vorbei

(define-state-entering-function game-over host
  (setf (want-game-start host) nil))	; setze die Liste der Spielwilligen zurück

(defhandler game-start (game-over) (host)
  (unless (member sender (want-game-start host) :test (address-compare-function host))
    (push sender (want-game-start host))) ; vermerke, dass der Spieler game-start gesendet hat
  (if (null (set-difference (want-game-start host) (registered-players host)))
      (switch-state host 'bidding-1)))

(in-package skat-kernel)

;; Konventionen:
;; alle eigentlichen Testfunktionen nehmen als Parameter:
;; 1. eine Liste mit drei Spielerkernen
;; 2. einen Hostkern
;; und sie geben diese beiden als values auch wieder zurück

(defun make-test-player ()
  (let ((player (make-instance 'player :ui (make-instance 'ui:stub-ui) :comm (make-instance 'comm::stub-comm))))
    (setf (ui::kernel (ui player)) player)
    (comm:start (comm player))		; Comm starten
    player))

(defmacro -send (kernel request &rest args)
  `(ui::send-request-to-kernel (ui ,kernel) ',request ,@args))

(defmacro -do (kernel)
  `(ui:just-one-step (ui ,kernel)))

(defun ui-send (receiving-kernel request &rest args)
  "Simuliert eine Benutzeraktion bei der UI des Kernels für den Test"
  (handler-bind ((comm::stub-communication-send #'stub-communication-send-testhandler)) ; Nachrichten zustellen
    (apply #'ui::send-request-to-kernel (ui receiving-kernel) request args)))

(defun ui-received-request-names (player)
  "Gibt die Namen aller empfangenen Anfragen der UI des Kernels zurück."
  (mapcar #'car (ui::received-requests (ui player))))

(defun assert-received (request player)
  "Setzt voraus, dass eine bestimmte Anfrage bei der UI angekommen ist"
  (assert (member request (ui-received-request-names player)) ()
	  "~a müsste ~a empfangen haben, dies ist aber nicht der Fall~%~
Liste der empfangengen Sachen: ~%~s" player request (ui-received-request-names player)))

(defun init-test-set ()
  "Erstellt einen Satz Spieler mit einem Host"
  (let ((players (list (make-test-player) (make-test-player) (make-test-player)))
	(host (make-test-host)))
    (values players host)))

(defun test-game-before-bidding (players host)
  (labels ((clear-requests (player)
	     (setf (ui::received-requests (ui player)) nil))
	   (update-entities ()
	     "Lässt jede UI ausstehende Anfragen verarbeiten"
	     (update-kernels (cons host players))))
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
      (values players host))))

(defun find-kernel-of-stub-comm (stub-comm kernels)
  (loop for kernel in kernels
     when (eq stub-comm (comm kernel)) return kernel))

(deftest "find-kernel-of-stub-comm" :category "player-tests"
	 :input-fn #'(lambda ()
		       (defparameter p1 (make-test-player) "Spielerinstanz für einige Testfälle")
		       (values (kern::comm p1) (list p1 (make-test-player))))
	 :test-fn #'find-kernel-of-stub-comm
	 :output-form (symbol-value 'p1))

(defmacro find-players-according-to-their-roles (players host (bidder listener other) &body body)
  "Bindet die Variablen in der dritten Argumentengruppe an die Kernel der
entsprechenden Spieler"
  `(let* ((,bidder (find-kernel-of-stub-comm (current-bidder ,host) ,players))
	  (,listener (find-kernel-of-stub-comm (current-listener ,host) ,players))
	  (,other (find-if #'(lambda (player) (not (or (eq player ,bidder)
						       (eq player ,listener))))
			   ,players)))
     ,@body))  

(defun ready-for-bidding-p (players host)
  "Gibt t zurück, wenn alle bereit zum Reizen sind."
  ;; Host muss Rollen vergeben haben
  (assert (not (null (current-dealer host))))
  (assert (not (null (current-bidder host))))
  (assert (not (null (current-listener host))))
  ;; Kernel zu diesen Adressen finden
  (find-players-according-to-their-roles players host (bidder listener dealer)
    (assert (eq (current-dealer host) (own-address dealer)))
    ;; die müssen in den richtigen Zuständen sein
    (assert-received 'ui:start-bidding bidder)
    (assert-state 'bid bidder)
    (assert-received 'ui:listen listener)
    (assert-state 'listen listener)
    (assert-state 'bidding-wait dealer)
    (assert (eq (bidding-mate listener) (own-address bidder)))
    (assert (eq (bidding-mate bidder) (own-address listener))))
  ;; Host muss Reizwerte führen und die müssen bei 18 losgehen
  (assert (not (null (bidding-values host))))
  (assert (= (car (bidding-values host)) 18))
  (assert (= (length (skat host)) 2))	; Host muss den Skat haben
  (dolist (player players)
    ;; Spieler dürfen den Skat nicht, also genau 10 Karten, haben
    (assert (= (length (cards player)) 10))
    ;; Spieler müssen auch Reizwerte führen, die bei 18 beginnen
    (assert (not (null (bidding-values player))))
    (assert (= (car (bidding-values player)) 18)))
  (values players host))

(defun bid-and-pass (players host who-passes)
  "Verallgemeinerung folgender Prozedur: Bidder reizt,
Listener joint und dann passt als nächstes der, der laut drittem
Parameter passen soll.
Führt am Ende keine Zustands-Assertions aus, aber am Anfang."
  (labels ((update-entities ()
	     (update-kernels (cons host players))))
    (find-players-according-to-their-roles players host (bidder listener other)
      (let ((host-bidding-state (state host)))
	(labels ((assert-same-roles ()
		   "setzt voraus, dass die Spieler immer noch die gleichen Rollen haben"
		   (assert-state host-bidding-state host)
		   (assert-state 'bid bidder)
		   (assert-state 'listen listener)
		   (assert-state 'bidding-wait other))
		 (bidder-bids ()
		   "lässt den Sager einen Reizwert sagen"
		   (ui-send bidder 'bid (car (bidding-values host)))
		   (update-entities)
		   ;; muss jeder mitbekommen haben
		   (assert-received 'ui:bid other)
		   (assert-received 'ui:reply-to-bid listener)
		   (assert-same-roles)))
	  (assert-state 'bid bidder)
	  (assert-state 'listen listener)
	  (assert-state 'bidding-wait other)
	  
	  (bidder-bids)

	  (ui-send listener 'join (car (bidding-values host)))
	  (update-entities)
	  (dolist (player (list bidder other))
	    (assert-received 'ui:join player))
	  ;; gleiche Rollen
	  (assert-state host-bidding-state host)
	  (assert-state 'bid bidder)
	  (assert-state 'listen listener)
	  (assert-state 'bidding-wait other)

	  (if (eq bidder who-passes)
	      (progn
		(ui-send bidder 'pass (car (bidding-values host)))
		(update-entities)
		;; muss jeder mitbekommen haben
		(dolist (player (list listener other))
		  (assert-received 'ui:pass player)))
	      (progn
		(bidder-bids)
		(ui-send listener 'pass (car (bidding-values host)))
		(update-entities)
		;; muss jeder mitbekommen haben
		(dolist (player (list bidder other))
		  (assert-received 'ui:pass player)))))))))

(defun assert-bidding-over (players host declarer)
  "Stellt sicher, dass alle mitbekommen haben, dass das Reizen vorbei ist."
  (dolist (player players)
    (assert-received 'ui:declarer player))		       ; DECLARER beendet das Reizen
  (assert-state 'declarer-found host)
  ;; sicherstellen, dass alle wissen, wer spielt
  (assert (eq (current-declarer host) (own-address declarer)))
  (dolist (player players)
    (eq (declarer player) (own-address declarer))
    (assert-state 'preparations player)))

(defun test-bidding-scenario_pass-bidder-dealer (players host)
  "Testet die Reizprozedur"
  (labels ((update-entities ()
	     (update-kernels (cons host players))))
    (assert (ready-for-bidding-p players host))
    (find-players-according-to-their-roles players host (bidder listener dealer)
      ;; Stub-Msgs besser lesbar machen
      (setf (comm::id (comm bidder)) '#:comm-bidder)
      (setf (comm::id (comm listener)) '#:comm-listener)
      (setf (comm::id (comm dealer)) '#:comm-dealer)
      
      (bid-and-pass players host bidder) ; Sager passt beim zweiten Mal
      ;; Rollen verändert:
      (assert-state 'bidding-2 host)
      (assert-state 'bidding-wait bidder)
      ;; Geber sagt weiter
      (assert (eq (current-bidder host) (own-address dealer)))
      (assert-state 'bid dealer)
      (assert-received 'ui:start-bidding dealer)
      ;; Hörer hört weiter
      (assert (eq (current-listener host) (own-address listener)))
      (assert-received 'ui:listen listener)
      (assert-state 'listen listener)
      (dolist (player players)
	(setf (ui::received-requests (ui player)) nil)) ; Log zurücksetzen

      (bid-and-pass players host dealer)	; Weitersager passt beim zweiten Mal
      ;; Rollen wieder verändert: Hörer entscheidet nun über Ramschen
      (assert-received 'ui:start-bidding listener)
      (assert-state 'bidding-3 host)
      (assert-state 'bidding-wait bidder)
      (assert-state 'bidding-wait dealer)
      (assert-state 'bid listener)
      (dolist (player players)
	(setf (ui::received-requests (ui player)) nil)) ; Log zurücksetzen

      (ui-send listener 'bid 18)	; kein Ramsch
      (update-entities)
      (dolist (kernel (list dealer bidder))
	(assert-received 'ui:bid kernel))
      (assert-bidding-over players host listener)))
  (values players host))


(deftest "before bidding" :category "player-tests"
	 :test-fn #'test-game-before-bidding
	 :input-form (init-test-set)
	 :compare-fn #'always-true)	; es geht nur um die Fehler während der Ausführung

(deftest "ready for bidding" :category "player-tests"
	 :test-fn #'ready-for-bidding-p
	 :input-form (apply #'test-game-before-bidding (multiple-value-list (init-test-set)))
	 :compare-fn #'always-true)

(deftest "bidding scenario 1" :category "player-tests"
	 :test-fn #'test-bidding-scenario_pass-bidder-dealer
	 :input-form (apply #'test-game-before-bidding (multiple-value-list (init-test-set)))
	 :compare-fn #'always-true)
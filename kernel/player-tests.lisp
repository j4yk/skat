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

(defun make-test-host ()
  (let ((host (make-instance 'host :ui (make-instance 'ui:host-ui) :comm (make-instance 'comm::stub-comm) :login-data nil)))
    (setf (ui::kernel (ui host)) host)
    (comm:start (comm host))		; Comm starten
    host))

(defun init-test-set ()
  "Erstellt einen Satz Spieler mit einem Host"
  (let ((players (list (make-test-player) (make-test-player) (make-test-player)))
	(host (make-test-host)))
    (values players host)))

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

(defun assert-state (asserted-state kernel)
  "Setzt einen bestimmten Zustand beim Spieler voraus."
  (assert (eq (state kernel) asserted-state) ()
	  "~a sollte in ~a sein, ist aber in ~a" kernel asserted-state (state kernel)))

(defun update-kernels (kernels)
  "Lässt jede UI ausstehende Anfragen verarbeiten"
  (handler-bind ((comm::stub-communication-send #'stub-communication-send-testhandler))
    (dolist (kernel kernels)
      (ui:just-one-step (ui kernel)))))

(defun stub-communication-send-testhandler (condition)
  "Condition-Handler. Stellt eine Anfrage von der einen zur anderen Stub-Comm zu
und ruft den Continue-Restart auf."
  (declare (type comm::stub-communication-send condition))
  (comm::push-request (comm::address condition)
		      (comm::sender-comm condition) (comm::request-name condition)
		      (comm::args condition))
  (continue))

(defun assert-received (request player)
  "Setzt voraus, dass eine bestimmte Anfrage bei der UI angekommen ist"
  (assert (member request (ui-received-request-names player)) ()
	  "~a müsste ~a empfangen haben, dies ist aber nicht der Fall~%~
Liste der empfangengen Sachen: ~%~s" player request (ui-received-request-names player)))

(defun find-kernel-of-stub-comm (stub-comm kernels)
  "Findet aus einer Liste von Kerneln denjenigen raus, zu dem die angegebene Stub-Comm gehört."
  (loop for kernel in kernels
     when (eq stub-comm (comm kernel)) return kernel))

(defvar p1 nil "Spielerinstanz für einige Testfälle")
(deftest "find-kernel-of-stub-comm" :category "player-tests"
	 :input-fn #'(lambda ()
		       (defparameter p1 (make-test-player))
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

(defun test-game-before-bidding (players host)
  "Testet den Spielverlauf vor dem Reizen"
  (labels ((update-entities ()
	     "Lässt jede UI ausstehende Anfragen verarbeiten"
	     (update-kernels (cons host players))))
    (update-entities) 		; erstes Mal, login-parameters muss ankommen
    (dolist (p players)
      (assert-received 'ui:login-parameters p)
      (assert-state 'start p)		; alle in Start
      (ui-send p 'login-data nil))
    (reset-received-requests players)
    (assert-state 'registration host)	; Host muss Registrierungen annehmen
    (update-entities)		; einloggen, registration-struct muss kommen
    (dolist (p players)
      (assert (slot-boundp p 'own-address))
      (assert-state 'unregistered p)
      (assert-received 'ui:registration-struct p)
      (ui-send p 'registration-data (comm::make-stub-registration-data :host-comm (comm host))) ; registieren
      (assert-state 'registration-pending p))
    (update-entities)		; hierbei müsste Host antworten und die Leute registrieren
    (assert (= (length (registered-players host)) 3) () "Host hat keine drei Spieler registriert")
    (dolist (p players)
      (assert-state 'registration-succeeded p) ; Registrierung war wohl erfolgreich
      (assert (slot-boundp p 'host)) ; Host muss bekannt sein
      (assert-received 'ui:registration-reply p)
      (assert-received 'ui:playmates p) ; Mitspieler wurden verkündet
      (assert (slot-boundp p 'left-playmate))
      (assert (slot-boundp p 'right-playmate))
      (ui-send p 'game-start))	; Spiel starten
    (update-entities)		; Host startet Spiel, teilt Karten aus und benennt Reizrollen
    (assert (ready-for-bidding-p players host)))
  (values players host))

(deftest "before bidding" :category "player-tests"
	 :test-fn #'test-game-before-bidding
	 :input-form (init-test-set)
	 :compare-fn #'always-true)	; es geht nur um die Fehler während der Ausführung

(defun bidding-mates-correct-p (bidder listener)
  "Gibt t zurück, wenn die bidding-mate Slots richtig gesetzt sind."
  (and (eq (bidding-mate listener) (own-address bidder))
       (eq (bidding-mate bidder) (own-address listener))))

(defun ready-for-bidding-p (players host)
  "Testet, ob alles zum Reizen bereit ist."
  ;; noch darf keiner als Spielführer vorgemerkt sein
  (assert (not (slot-boundp host 'current-declarer)) ()
	  "Host hat seinen vorigen Declarer nicht gelöscht")
  ;; Host muss Rollen vergeben haben
  (assert (not (null (current-dealer host))))
  (assert (not (null (current-bidder host))))
  (assert (not (null (current-listener host))))
  (dolist (player players)
    (assert-received 'ui:game-start player) ; vom Host
    (assert-received 'ui:cards player)	    ; Karten erhalten
    ;; Spieler dürfen den Skat nicht, also genau 10 Karten, haben
    (assert (= (length (cards player)) 10))
    ;; Spieler müssen auch Reizwerte führen, die bei 18 beginnen
    (assert (not (null (bidding-values player))))
    (assert (= (car (bidding-values player)) 18)))
  ;; Kernel zu den Reizrollen finden
  (find-players-according-to-their-roles players host (bidder listener dealer)
    ;; bidder listener wurden anhand von (current-bidder) und (current-listener) ermittelt,
    ;; deshalb brauchen die hier nicht mehr überprüft zu werden
    (assert (eq (current-dealer host) (own-address dealer)))
    ;; die müssen in den richtigen Zuständen sein
    (assert-received 'ui:start-bidding bidder)
    (assert-state 'bid bidder)
    (assert-received 'ui:listen listener)
    (assert-state 'listen listener)
    (assert-state 'bidding-wait dealer)
    ;; und die Reizpartner müssen sich kennen
    (assert (bidding-mates-correct-p bidder listener)))
  ;; Host muss Reizwerte führen und die müssen bei 18 losgehen
  (assert (not (null (bidding-values host))))
  (assert (= (car (bidding-values host)) 18))
  (assert (= (length (skat host)) 2))	; Host muss den Skat haben
  (values players host))

(deftest "ready for bidding" :category "player-tests"
	 :test-fn #'ready-for-bidding-p
	 :input-form (apply #'test-game-before-bidding (multiple-value-list (init-test-set)))
	 :compare-fn #'always-true)

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
	      ;; der momentane Sager soll passen
	      (let-pass players host bidder (list listener other))
	      ;; der momentane Hörer soll passen
	      (progn
		(bidder-bids)		; damit der Hörer passen kann, muss der Sager nochwas reizen
		(let-pass players host listener (list bidder other)))))))))

(defun let-pass (players host who-passes the-others)
  "Lässt den Spieler passen und stellt sicher, dass die anderen Spieler dies mitbekommen"
  (ui-send who-passes 'pass (car (bidding-values host)))
  (update-kernels (cons host players))
  ;; muss jeder mitbekommen haben
  (dolist (player the-others)
    (assert-received 'ui:pass player)))

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

(defun assert-bidding-configuration (bidder listener other host)
  "Setzt voraus, dass die Rollen wie angegeben verteilt sind."
  (assert (eq (current-bidder host) (own-address bidder)))
  (assert-received 'ui:start-bidding bidder)
  (assert-state 'bid bidder)
  (assert (eq (current-listener host) (own-address listener)))
  (assert-received 'ui:listen listener)
  (assert-state 'listen listener)
  (assert (bidding-mates-correct-p bidder listener))
  (assert-state 'bidding-wait other))

(defmacro define-bidding-scenario (name docstring &body body)
  "Definert eine Testfunktion für ein Reizszenario.
Die Variablen bidder listener und dealer sind für body gebunden."
  `(progn
     (defun ,name (players host)
       ,docstring
       (assert (ready-for-bidding-p players host))
       (find-players-according-to-their-roles players host (bidder listener dealer)
	 ;; Stub-Msgs besser lesbar machen
	 (if (or (>= ui::*stub-ui-verbosity* 1) (>= comm::*stub-comm-verbosity* 1))
	     (format t "~%==== RENAMING the comms ===="))
	 (setf (comm::id (comm bidder)) '#:comm-bidder)
	 (setf (comm::id (comm listener)) '#:comm-listener)
	 (setf (comm::id (comm dealer)) '#:comm-dealer)
	 ,@body))
     
     (deftest ,(symbol-name name) :category "player-tests"
	      :test-fn #',name
	      :input-form (apply #'test-game-before-bidding (multiple-value-list (init-test-set)))
	      :compare-fn #'always-true)))

(defun reset-received-requests (players)
  "Setzt die Listen der empfangenen Anfragen zurück."
  (dolist (player players)
    (setf (ui::received-requests (ui player)) nil)))

(defmacro two-pass-bidding-scenario (name who-passes-first who-passes-second who-declares)
  "Definiert ein Bidding-Szenario, bei dem zwei Spieler beim jeweils zweiten Mal passen"
  (dolist (p (list who-passes-first who-passes-second who-declares))
    (assert (member p '(bidder listener dealer)) () "Die angegebenen Rollen müssten bidder, listener oder dealer sein."))
  `(define-bidding-scenario ,name
       "Testet die Reizprozedur"
     (bid-and-pass players host ,who-passes-first) ; erster Pass beim zweiten Mal
     ;; Rollen verändert:
     (assert-state 'bidding-2 host)
     (assert-bidding-configuration dealer
				   ,(if (eq who-passes-first 'bidder)
					'listener
					'bidder)
				   ,(if (eq who-passes-first 'bidder)
					'bidder
					'listener)
				   host)
     (reset-received-requests players)

     (bid-and-pass players host ,who-passes-second)	; zweiter Pass bei zweitem Mal
     ;; kein Ramsch, da der declarer irgendwo schonmal gejoint ist (bid-and-pass)
     (assert-bidding-over players host ,who-declares)
     (values players host)))

(two-pass-bidding-scenario bidder-and-dealer-pass bidder dealer listener)

(two-pass-bidding-scenario listener-and-dealer-pass listener dealer bidder)

(two-pass-bidding-scenario bidder-and-listener-pass bidder listener dealer)

(two-pass-bidding-scenario listener-and-bidder-pass listener bidder dealer)

(defun assert-bidding-3-situation (bidder others host)
  "Setzt voraus, dass ein Spieler reizen soll, die anderen nur warten und
der Host im dritten Reizstadium ist."
  (assert-state 'bidding-3 host)
  (assert-received 'ui:start-bidding bidder)
  (assert-state 'bid bidder)
  (assert-state 'bidding-wait (car others))
  (assert-state 'bidding-wait (cadr others)))

(define-bidding-scenario bidder-and-dealer-pass-instantly
    "Testet den Reizverlauf, wenn Sager und Geber sofort passen, Hörer also über Ramsch
entscheiden kann"
  (let-pass players host bidder (list dealer listener))
  (assert-state 'bidding-2 host)
  (assert-bidding-configuration dealer listener bidder host)
  (reset-received-requests players)
  (let-pass players host dealer (list listener bidder))
  ;; jetzt hat noch keiner irgendwas gereizt
  ;; Hörer muss entscheiden, ob Ramsch oder nicht
  (assert-bidding-3-situation listener (list bidder dealer) host)
  (ui-send listener 'bid 18)		; kein Ramsch
  (update-kernels (cons host players))
  (dolist (player (list bidder dealer))
    (assert-received 'ui:bid player))	; die anderen haben es mitbekommen
  (assert-bidding-over players host listener)
  (values players host))

(define-bidding-scenario everyone-passes
    "Testet den Reizverlauf für den Fall, dass keiner Spielen will"
  (let-pass players host bidder (list dealer listener))
  (assert-state 'bidding-2 host)
  (assert-bidding-configuration dealer listener bidder host)
  (reset-received-requests players)
  (let-pass players host dealer (list listener bidder))
  ;; jetzt hat noch keiner irgendwas gereizt
  ;; Hörer muss entscheiden, ob Ramsch oder nicht
  (assert-bidding-3-situation listener (list bidder dealer) host)
  (ui-send listener 'pass 18)
  (update-kernels (cons host players))	; Runde abblasen
  (dolist (player (list bidder dealer))
    (assert-received 'ui:pass player))
  (assert-game-over players host t)	; Spiel vorbei, aber keine Spielergebnisse
  (values players host))

;; soviel zum Thema Reizen...

(defun test-game (players host)
  "Testfall für eine Spielrunde (die Spieler spielen immer ihre ganz linke Karte an)"
  (labels ((update-entities ()
	     (update-kernels (cons host players))))
    (let* ((declarer (find-kernel-of-stub-comm (current-declarer host) players))
	   (forehand (find-kernel-of-stub-comm (current-forehand host) players))
	   (other-players (set-difference players (list declarer))))
      (assert-bidding-over players host declarer)
      ;; alle haben DECLARER bekommen
      (ui-send declarer 'hand-decision nil) ; will den Skat haben
      (update-entities)
      (assert-state 'skat-away host)
      (dolist (player other-players)
	(assert-received 'ui:hand-decision player))
      (assert-received 'ui:skat declarer)
      (assert (= (length (cards declarer)) 12)) ; Skat auf der Hand
      (assert (not (slot-boundp host 'skat)))	; Host hat ihn nicht
      ;; die ersten beiden Karten drücken
      (ui-send declarer 'skat (list (first (cards declarer)) (second (cards declarer))))
      (update-entities)
      (assert (= (length (cards declarer)) 10)) ; Skat nicht mehr auf der Hand
      (assert (= (length (skat host)) 2))	; sondern beim Host
      (assert-state 'await-declaration host)
      (reset-received-requests players)
      ;; Ansage
      (ui-send declarer 'declaration (list :grand))
      (update-entities)
      (assert-state 'in-game host)
      (assert (equal (declarer-declaration host) (list :grand)) () "~s != (:grand) (vor dotimes, host)" (declarer-declaration host))
      (dolist (player other-players)
	(assert-received 'ui:declaration player))
      (dolist (player players)
	(assert-state 'in-game player)
	(assert (equal (game-declaration player) (list :grand)) () "~s != (:grand) (vor dotimes)" (game-declaration player)))
      ;; Vorderhand kommt raus
      (assert (eq (current-player host) (comm forehand)))
      (assert (eq (right-playmate forehand) (current-dealer host)))
      (labels ((send-card (player)
		 (ui-send player 'card (car (cards player)))
		 (update-entities))
	       (assert-trick-length (length)
		 (assert (= (length (trick-contributions (current-trick host))) length))
		 (dolist (player players)
		   (assert (= (length (current-trick player)) length)))))
	(dotimes (turn 10)
	  (let* ((forehand (find-kernel-of-stub-comm (current-player host) players))
		 (middlehand (find-kernel-of-stub-comm (left-playmate forehand) players))
		 (backhand (find-kernel-of-stub-comm (right-playmate forehand) players)))
	    (assert (equal (slot-value host 'declaration) (list :grand)) ()
		    "~s != (:grand) (in dotimes)" (declarer-declaration host))
	    (dolist (player players)
	      (assert (eq (current-player player) (comm forehand))))
	    (assert-received 'ui:choose-card forehand)
	    ;; erste Karte
	    (send-card forehand)
	    (dolist (player (list middlehand backhand))
	      (assert-received 'ui:card player))
	    (assert-received 'ui:choose-card middlehand)
	    (reset-received-requests players)
	    (assert-trick-length 1)
	    (dolist (player players)
	      (assert (eq (current-player player) (comm middlehand))))
	    ;; zweite Karte
	    (send-card middlehand)
	    (dolist (player (list backhand forehand))
	      (assert-received 'ui:card player))
	    (assert-received 'ui:choose-card backhand)
	    (reset-received-requests players)
	    (assert-trick-length 2)
	    (dolist (player players)
	      (assert (eq (current-player player) (comm backhand))))
	    ;; dritte Karte
	    (send-card backhand)
	    (dolist (player (list forehand middlehand))
	      (assert-received 'ui:card player))
	    (dolist (player players)
	      (assert-received 'ui:trick player))))
	;; Spiel vorbei
	(dolist (player players)
	  (assert-received 'ui:game-over player)
	  (assert-received 'ui:cards-score player)
	  (assert-received 'ui:game-result player)
	  (assert-received 'ui:score-table player))
	(reset-received-requests players))))
  (values players host))

(deftest "test game" :category "player-tests"
	 :input-fn #'(lambda () (multiple-value-call #'bidder-and-dealer-pass (multiple-value-call #'test-game-before-bidding (init-test-set))))
	 :test-fn #'test-game
	 :compare-fn #'always-true)
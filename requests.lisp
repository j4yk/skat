(in-package skat-requests)

(defvar *request-definitions* nil "Assoc Liste: (cons :request-name (list of :parameter-names))")
	
(defun add-request-definition (name &rest parameters)
  "Eine Request-Definition in *request-definitions* einfügen"
  (push (cons (to-keyword name) (mapcar #'to-keyword parameters)) *request-definitions*))

(defun request-exists-p (name)
  "Gibt nil zurück, wenn Anfragen mit diesem Namen nicht, sonst einen Wert verschieden von nil"
  (assoc (to-keyword name) *request-definitions*))

(defun request-parameters (request-name)
  "Gibt die Parameterliste (keywords) eines Requests zurück."
  (cdr (assoc (to-keyword request-name) *request-definitions*)))
     
(defun correct-parameters-p (name &rest parameters)
  "Gibt t zurück, wenn die Namen der Parameter und ihre Reihenfolge mit denen in der
Definition des Requests übereinstimmen."
  (equal (mapcar #'to-keyword parameters) (cdr (assoc (to-keyword name) *request-definitions*))))

(eval-when (:compile-toplevel)
  (defun format-parameters-list-latex (parameter-definitions)
    (labels ((format-parameter-item (param-def)
	       (let ((name (car param-def))
		     (doc (cadr param-def)))
		 (format nil "~a: ~a" name doc))))
      (format nil "\\begin{requestsparameterslist}
~{  \\item ~a~%~}\\end{requestsparameterslist}" (mapcar #'format-parameter-item parameter-definitions))))

  (defun print-request-latex (name parameter-definitions documentation sender receiver stream)
    "Formatiert eine Requestdefinition für die LaTeX-Dokumentation."
    (declare (ignore sender))
    (format stream "\\request~%  {~a}~%  {~a}~%  {~a}~%  {~a}~%"
	    name
	    documentation
	    (if (null parameter-definitions)
		"-"
		(format-parameters-list-latex parameter-definitions))
	    ;; sender
	    receiver)))

(defmacro defrequest (name parameters options)
  "Definiert eine neue Art Request.

defrequest name parameter*

name: Name des Requests. Daran orientieren sich die Handler-Funktionen zur Unterscheidung verschiedener Request-Arten.
parameter: Name eines dem Request immer zwingend beigefügten Parameters"
  (declare (ignorable options))
  (let ((parameter-symbols (mapcar #'car parameters))
	(documentation (nth (1+ (position :documentation options)) options))
	(sender (nth (1+ (position :sender options)) options))
	(receiver (nth (1+ (position :receiver options)) options)))
    `(progn
       (eval-when (:compile-toplevel)
	 ;; create Documentation
	 (with-open-file (fs "../skat-doc/requests-table.tex"
			     :direction :output
			     :if-exists (if (boundp '*request-printing-started*)
					    (print :append)
					    (print :supersede)))
	   (print-request-latex ',name ',parameters ,documentation ,sender ,receiver fs)
	   (format t "printed request ~a" ',name))
	 (defvar *request-printing-started* t))
       (eval-when (:load-toplevel :execute)
	 (apply #'add-request-definition ',name ',parameter-symbols)))))

(define-condition %request-error (error)
  ((request-name :accessor request-name :initarg :request-name)))

(define-condition wrong-request-parameters (%request-error) ())
(define-condition undefined-request-error (%request-error) ())

(makunbound '*request-printing-started*)

(DEFREQUEST LOGIN-PARAMETERS
    ((PARAMETERS
      "Eine Liste von Assoziationen $Parametername \\rightarrow Datentyp$"))
  (:DOCUMENTATION
   "Enthält eine Liste notwendiger Informationen für die Einwahl des Kommunikationsmoduls in seinem Kommunikationskanal"
   :SENDER "Kommunikation" :RECEIVER "UI"))

(DEFREQUEST LOGIN-DATA
    ((DATA
      "Eine Liste von Assoziationen $Parametername \\rightarrow Parameterwert$"))
  (:DOCUMENTATION
   "Enthält die der Kommunikation zur Verfügung gestellten Einwahldaten."
   :SENDER "UI" :RECEIVER "Kommunikation"))

(DEFREQUEST OWN-ADDRESS ((address ""))
  (:DOCUMENTATION
   "Teilt dem Kernel die eigene Adresse mit (die hängt ja vom Kommunikationsmodul ab)."
   :SENDER "Kommunikation" :RECEIVER "Kernel"))

(DEFREQUEST REGISTRATION-PARAMETERS
    ((PARAMETERS
      "Eine Liste von Assoziationen $Parametername \\rightarrow Datentyp$"))
  (:DOCUMENTATION
   "Enthält eine Liste notwendiger Informationen für die Registrierung mit einem Host"
   :SENDER "Kommunikation" :RECEIVER "UI"))

(DEFREQUEST REGISTRATION-DATA
    ((DATA
      "Eine Liste von Assoziationen $Parametername \\rightarrow Parameterwert$"))
  (:DOCUMENTATION
   "Enthält die der Kommunikation zur Verfügung gestellten Daten zur Registrierung mit einem Host."
   :SENDER "UI" :RECEIVER "Kommunikation"))

(DEFREQUEST REGISTRATION-REQUEST
    NIL
  (:DOCUMENTATION "Fragt an, ob der Sender sich beim Host eintragen darf."
		  :SENDER "Player" :RECEIVER "Host"))

(DEFREQUEST REGISTRATION-REPLY
    ((ACCEPTED
      "t bedeutet, der Spieler ist eingetragen, nil bedeutet, dass die Registrierung abgelehnt ist"))
  (:DOCUMENTATION
   "Die Antwort des Hostes auf eine registration-request (Ja oder Nein)."
   :SENDER "Host" :RECEIVER "Player"))

(DEFREQUEST SERVER-UPDATE ((EVENTS ""))
  (:DOCUMENTATION
   "Statusmeldung des Hostes, die wartende Spieler über Neuigkeiten informiert."
   :SENDER "Host" :RECEIVER "Player"))

(DEFREQUEST UNREGISTER
    NIL
  (:DOCUMENTATION
   "Teilt dem Host mit, dass der Spieler die Runde verlassen hat." :SENDER
   "Player" :RECEIVER "Host"))

(DEFREQUEST LOGOUT
    ((ADDRESS "Adresse des Spielers oder Hosts, der sich ausgeloggt hat"))
  (:DOCUMENTATION
   "Interne Nachricht, dass ein Mitspieler auf Kommunikationsebene (z. B. XMPP) ausgeloggt wurde."
   :SENDER "Kommunikation" :RECEIVER "Kernel, UI"))

(DEFREQUEST PLAYMATES
    ((LEFT "Adresse des linken Mitspielers")
     (RIGHT "Adresse des rechten Mitspielers"))
  (:DOCUMENTATION "Informiert einen Spieler über seine Mitspieler" :SENDER
		  "Host" :RECEIVER "Player"))

(DEFREQUEST GAME-START
    NIL
  (:DOCUMENTATION
   " 1. Möglichkeit: Host informiert Spieler über Spielbeginn
2. Möglichkeit: Spieler drückt Bereitschaft zum nächsten Spiel aus"
   :SENDER "Player, Host" :RECEIVER "Host, Player"))

(DEFREQUEST CARDS
    ((CARDS "Liste von Karten"))
  (:DOCUMENTATION "Eine Menge von Karten, die ausgeteilt werden." :SENDER
		  "Host" :RECEIVER "Player"))

(DEFREQUEST START-BIDDING
    ((LISTENER "Die Adresse des zuhörenden Spielers")
     (MIN-VALUE "Mindestreizwert"))
  (:DOCUMENTATION "Teilt einem Spieler mit, dass er Reizwerte ansagen soll."
		  :SENDER "Host" :RECEIVER "Player"))

(DEFREQUEST LISTEN
    ((BIDDER "Die Adresse des Reizwerte sagenden Spielers"))
  (:DOCUMENTATION "Teilt einem Spieler mit, dass er dem Bidder zuhören soll."
		  :SENDER "Host" :RECEIVER "Player"))

(DEFREQUEST BID
    ((VALUE "der Reizwert"))
  (:DOCUMENTATION "Ansage eines Reizwertes" :SENDER "Player" :RECEIVER
		  "Player, Host"))

(DEFREQUEST JOIN
    ((VALUE "der Reizwert"))
  (:DOCUMENTATION "Mitgehen bei einem Reizwert" :SENDER "Player," :RECEIVER
		  "Player, Host"))

(DEFREQUEST PASS
    ((VALUE "der Reizwert"))
  (:DOCUMENTATION "Bei einem Reizwert passen" :SENDER "Player," :RECEIVER
		  "Player, Host"))

(DEFREQUEST DECLARER
    ((DECLARER "Adresse des Spielführers"))
  (:DOCUMENTATION "Verkündet den Spieler, der das Spiel führt" :SENDER "Host"
		  :RECEIVER "Player"))

(DEFREQUEST HAND-DECISION
    ((HAND "wenn t, spielt der Spieler ein Handspiel, sonst möchte er den Skat"))
  (:DOCUMENTATION
   "Verkündet die Entscheidung des Declarers, ob er Hand spielt oder nicht"
   :SENDER "Player" :RECEIVER "Host, Player"))

(DEFREQUEST SKAT
    ((SKAT "Liste mit zwei Karten"))
  (:DOCUMENTATION
   "Die Übergabe des Skats. Sowohl vom Host zum Declarer als auch zurück."
   :SENDER "Host, Player" :RECEIVER "Host, Player"))

(defrequest flush-run
    ((with-or-without
      "entweder :with oder :without (mit oder ohne)")
     (run-value
      "Die Anzahl der (fehlenden) Trumpfspitzen"))
  (:documentation "Wird durch den Spielführer vor DECLARATION übermittelt, um den Spielwert auszurechnen"
		  :sender "Player" :receiver "Host"))

(DEFREQUEST DECLARATION
    ((DECLARATION "Eine Liste an Spieloptionen (zum Beispiel was Trumpf ist)"))
  (:DOCUMENTATION "Verkündet das Spiel, das der Declarer ansagt" :SENDER
		  "Player" :RECEIVER "Host, Player"))

(DEFREQUEST CARD
    ((CARD "eine Karte"))
  (:DOCUMENTATION "Eine gespielte Karte." :SENDER "Player" :RECEIVER
		  "Player, Host"))

(DEFREQUEST CHOOSE-CARD
    NIL
  (:DOCUMENTATION "Fordert den Spieler auf, eine Karte zu spielen" :SENDER
		  "Host" :RECEIVER "Player"))

(DEFREQUEST TRICK
    ((CARDS "die drei Karten des Stichs")
     (WINNER "Adresse des Spielers, der den Stich mitnimmt"))
  (:DOCUMENTATION
   "Fasst einen vollständigen Stich zusammen und teilt den Gewinner mit"
   :SENDER "Host" :RECEIVER "Player"))

(DEFREQUEST GAME-OVER
    ((PROMPT
      "wenn t, dann können die Spieler entscheiden, ob sie noch eine Runde spielen wollen"))
  (:DOCUMENTATION "Offizielle Beendigung des Spiels durch den Host" :SENDER
		  "Host" :RECEIVER "Player"))

(DEFREQUEST CARDS-SCORE
    ((DECLARER-SCORE "die Augensumme der Spielführerstiche")
     (DEFENDERS-SCORE "die Augensumme der Stiche der verteidigenden Spieler"))
  (:DOCUMENTATION "Informiert die Spieler über die gewonnenen Augenpunkte"
		  :SENDER "Host" :RECEIVER "Player"))

(DEFREQUEST GAME-RESULT
    ((DECLARATION "Liste mit dem, was der Spielführer angesagt hat")
     (WON
      "wenn t, hat der Spielführer gewonnen, wenn nil, haben die verteidigenden Spieler gewonnen")
     (SCORE "der Punktewert dieses Spieles"))
  (:DOCUMENTATION "Fasst das Spielresultat zusammen." :SENDER "Host" :RECEIVER
		  "Player"))

(DEFREQUEST SCORE-table
    ((PLAYER1-ADDRESS "Adresse von Spieler 1")
     (PLAYER1-SCORE "Punktestand von Spieler 1")
     (PLAYER2-ADDRESS "analog")
     (PLAYER2-SCORE "analog")
     (PLAYER3-ADDRESS "analog")
     (PLAYER3-SCORE "analog"))
  (:DOCUMENTATION
   "Die Punktestände der Spieler nach einem Spiel" :SENDER
   "Host" :RECEIVER "Player"))

(DEFREQUEST MESSAGE
    ((TEXT "Nachrichtentext"))
  (:DOCUMENTATION "Eine Textsendung an einen Spieler" :SENDER "Host, Player"
		  :RECEIVER "Player"))

;; (defrequest login-parameters parameters)
;; (defrequest login-data data)
;; (defrequest own-address address)
;; (defrequest registration-parameters parameters)
;; (defrequest registration-data data)
;; (defrequest registration-request)
;; (defrequest registration-reply accepted)
;; (defrequest server-update events)
;; (defrequest unregister)
;; (defrequest logout)
;; (defrequest playmates left right)
;; (defrequest game-start)
;; (defrequest cards cards)
;; (defrequest start-bidding listener min-value)
;; (defrequest listen bidder)
;; (defrequest bid value)
;; (defrequest join value)
;; (defrequest pass value)
;; (defrequest declarer declarer)
;; (defrequest hand-decision hand)
;; (defrequest skat skat)
;; (defrequest declaration declaration)
;; (defrequest card card)
;; (defrequest choose-card)
;; (defrequest trick cards winner)
;; (defrequest game-over)
;; (defrequest cards-score declarer-score defenders-score)
;; (defrequest game-result declaration won score)
;; (defrequest match-score player1-address player1-score
;; 	    player2-address player2-score
;; 	    player3-address player3-score)
;; (defrequest game-end prompt)
;; (defrequest logout)

;; (defrequest message text)

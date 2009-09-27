(in-package skat-requests)

(defvar *request-definitions* nil "Assoc Liste: (cons :request-name (list of :parameter-names))")
	
(defun add-request-definition (name &rest parameters)
  "Eine Request-Definition in *request-definitions* einfügen"
  (push (cons (to-keyword name) (mapcar #'to-keyword parameters)) *request-definitions*))

(defun request-parameters (request-name)
  "Gibt die Parameterliste (keywords) eines Requests zurück."
  (cdr (assoc (to-keyword request-name) *request-definitions*)))

(defmacro defrequest (name &rest parameters)
  "Definiert eine neue Art Request.

defrequest name parameter*

name: Name des Requests. Daran orientieren sich die Handler-Funktionen zur Unterscheidung verschiedener Request-Arten.
parameter: Name eines dem Request immer zwingend beigefügten Parameters"
  `(apply #'add-request-definition ',name ',parameters))

(defun correct-parameters-p (name &rest parameters)
  "Gibt t zurück, wenn die Namen der Parameter und ihre Reihenfolge mit denen in der
Definition des Requests übereinstimmen."
  (equal (mapcar #'to-keyword parameters) (cdr (assoc (to-keyword name) *request-definitions*))))

(defrequest login-parameters parameters)
(defrequest login-data data)
(defrequest own-address address)
(defrequest registration-parameters parameters)
(defrequest registration-data data)
(defrequest registration-request)
(defrequest registration-reply accepted)
(defrequest server-update events)
(defrequest unregister)
(defrequest playmates left right)
(defrequest game-start)
(defrequest cards cards)
(defrequest start-bidding listener min-value)
(defrequest listen bidder)
(defrequest bid value)
(defrequest join value)
(defrequest pass value)
(defrequest declarer declarer)
(defrequest hand-decision hand)
(defrequest skat skat)
(defrequest declaration declaration)
(defrequest card card)
(defrequest choose-card)
(defrequest trick cards winner)
(defrequest game-over)
(defrequest cards-score declarer-scrore defenders-score)
(defrequest game-result declaration won score)
(defrequest match-score player1-address player1-score
	    player2-address player2-score
	    player3-address player3-score)
(defrequest game-end prompt)
(defrequest logout)

(in-package skat-kernel)

(defclass host (base-kernel)
  ((registered-players :accessor registered-players :initform nil)
   (current-dealer :accessor current-dealer)
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

(defhandler registration-request (registration) (host sender)
  (if (>= (length (registered-players host)) 3)
      (comm:send (comm host) sender registration-reply nil)
      (unless (member sender (registered-players host) :test (address-compare-function host))
	(send-to-players host server-update `(:player-join ,sender))
	(push sender (registered-players host)) ; Spieler aufnehmen
	(comm:send (comm host) sender registration-reply t))))

(defhandler game-start (game-over) (host sender)
  (unless (member sender (want-game-start host) :test (address-compare-function host))
    (push sender (want-game-start host)))
  (if (null (set-difference (want-game-start host) (registered-players host)))
      (switch-state host 'bidding-1)))
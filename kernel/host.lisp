(in-package skat-kernel)

(defclass host (base-kernel)
  ((registered-players :accessor registered-players :initform nil)
   (current-dealer :accessor current-dealer)
   (current-declarer :accessor current-declarer)
   (score-table :accessor score-table)
   (skat :accessor skat :documentation "Noch nicht ausgegebener Skat")
   (want-game-start :accessor want-game-start :initform nil 
		    :documentation "Liste der Spieler, die eine neue Runde wollen")))

(defhandler game-start (game-over) (host sender)
  (unless (member sender (want-game-start host) :test (address-compare-function host))
    (push sender (want-game-start host)))
  (if (null (set-difference (want-game-start host) (registered-players host)))
      (switch-state host 'bidding-1)))
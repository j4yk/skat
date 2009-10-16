(in-package :skat-kernel)

(defstruct trick contributions)

(defun add-contribution (card player trick)
  "Fügt dem Stich eine Karte von einem Spieler bei."
  (push (cons card player) (trick-contributions trick))
  trick)				; trick als Rückgabewert

(defmethod cards ((trick trick))
  "Gibt alle Karten in einem Stich zurück."
  (mapcar #'car (trick-contributions trick)))

(defun trick-winner-contrib (trick game-variant)
  (assoc (apply #'greatest-card game-variant
		     (mapcar #'car (reverse (trick-contributions trick))))
	      (trick-contributions trick)))

(defun trick-winner-card (trick game-variant)
  "Gibt die höchste Karte im Stich zurück."
  (car (trick-winner-contrib trick game-variant)))

(defun trick-winner (trick game-variant)
  "Gibt die Adresse des Stichsiegers zurück."
  (cdr (trick-winner-contrib trick game-variant)))

(defun trick-complete-p (trick)
  "Gibt zurück, ob der Stich komplett ist (3 Karten)"
  (= 3 (length (trick-contributions trick))))
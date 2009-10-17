(in-package :skat-kernel)

(defstruct trick contributions associated-winner-contrib)

(defun add-contribution (card player trick)
  "Fügt dem Stich eine Karte von einem Spieler bei."
  (push (cons card player) (trick-contributions trick))
  trick)				; trick als Rückgabewert

(defmacro make-trick-with-contributions (&rest args)
  "make-trick-with-contributions [card player]*
Erstellt einen Stich mit den als Argumente übergebenen
Beiträgen."
  (let ((args (copy-list args)))
    `(let ((trick (make-trick)))
       ,@(loop for card = (pop args)
	    for player = (pop args)
	    collect `(add-contribution ,card ,player trick)
	    until (< (length args) 2))
       trick)))

(defmethod cards ((trick trick))
  "Gibt alle Karten in einem Stich zurück."
  (mapcar #'car (trick-contributions trick)))

(defun trick-winner-contrib (trick game-variant)
  "Gibt die Kombination von Karte und Spieler zurück,
die den Stich für sich entscheidet."
  (when (trick-complete-p trick)	; geht nur für komplette Stiche
    (if (trick-associated-winner-contrib trick)
	;; wurde schonmal berechnet, also das zurückgeben
	(trick-associated-winner-contrib trick)
	;; ansonsten berechnen und merken
	(if game-variant
	    (setf (trick-associated-winner-contrib trick)
		  (assoc (apply #'greatest-card game-variant
				(mapcar #'car (reverse (trick-contributions trick))))
			 (trick-contributions trick)))
	    (error "No game variant specified, but required to compute trick winner!")))))

(defun trick-winner-card (trick &optional game-variant)
  "Gibt die höchste Karte im Stich zurück."
  (car (trick-winner-contrib trick game-variant)))

(defun trick-winner (trick &optional game-variant)
  "Gibt die Adresse des Stichsiegers zurück."
  (cdr (trick-winner-contrib trick game-variant)))

(defun trick-complete-p (trick)
  "Gibt zurück, ob der Stich komplett ist (3 Karten)"
  (= 3 (length (trick-contributions trick))))

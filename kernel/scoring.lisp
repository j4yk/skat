(in-package skat-kernel)

(defun game-variant (declaration)
  "Gibt die Spielvariante in einer Ansage zurück."
  (find-if #'(lambda (thing)
	       (find thing '(:grand :null :diamonds :hearts :spades :clubs :ramsch)))
	   declaration))

(defun game-points (declaration flush-run-value)
  "Gibt den Spielwert der Ansage zurück."
  (if (eq (car declaration) :null)
      (cond ((equal (cdr declaration) nil) 23)
	    ((equal (cdr declaration) '(:hand)) 35)
	    ((equal (cdr declaration) '(:ouvert)) 46)
	    ((equal (cdr declaration) '(:ouvert :hand)) 59))
      (* (ccase (car declaration)
	   (:diamonds 9)
	   (:hearts 10)
	   (:spades 11)
	   (:clubs 12)
	   (:grand 24))
	 (+ flush-run-value 1 (length (cdr declaration))))))

(deftests "Spielwert"
  ("Punkte für 3 Spitzen, Grand" (game-points '(:grand) 3) (* (+ 3 1) 24))
  ("Punkte für 2 Spitzen, Grand Hand" (game-points '(:grand :hand) 2) (* (+ 2 1 1) 24))
  ("Punkte für 1 Spitze, Karo, Hand, Schneider gespielt"
   (game-points '(:diamonds :played-schneider :hand) 1) (* (+ 1 1 1 1) 9))
  ("Punkte für 4 Spitzen, Herz, Hand, Schneider gespielt, Schneider angesagt, Schwarz gespielt"
   (game-points '(:hearts :played-schwarz :played-schneider :hand :declared-schneider) 4)
   (* (+ 4 1 1 1 1 1) 10))
  ("Punkte für 4 Spitzen, Grand Ouvert" (game-points '(:grand :played-schwarz :played-schneider
						       :hand :declared-schneider :declared-schwarz
						       :ouvert) 4)
					(* (+ 4 1 1 1 1 1 1 1) 24))
  ("Punkte für Null" (game-points '(:null) 3) 23)
  ("Punkte für Null Hand" (game-points '(:null :hand) 2) 35)
  ("Punkte für Null Ouvert" (game-points '(:null :ouvert) 1) 46)
  ("Punkte für Null Ouvert Hand" (game-points '(:null :ouvert :hand) 4) 59))

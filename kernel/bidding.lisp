(in-package :skat-kernel)

(defparameter *game-point-levels*
  (let ((values (append
		 (loop ; Farbspiel
		    for suit-value in '(9 10 11 12)
		    append (loop
			      for declaration-value in (loop for n from 2 to 18 collect n)
			      collect (* suit-value declaration-value)))
		 (loop with grand-value = 24 ; Grand
		    for declaration-value in (loop for n from 2 to 11 collect n)
		    collect (* grand-value declaration-value))
		 (list 23 35 46 59)))) ; Null
    (delete-duplicates (sort values #'<))))

(defun cut-away-game-point-levels (bid &optional (point-levels *game-point-levels*))
  "Gibt eine Liste zurück, aus der alle Reizwerte vor dem angegebenen gestrichen sind.
Das erste Element der zurückgegebenen Liste ist der angegebene Reizwert."
  (if (<= bid (car point-levels))
      point-levels
      (cut-away-game-point-levels bid (cdr point-levels))))

(deftests "Bidding"
  ("cut-away" (cut-away-game-point-levels 22) (cddr *game-point-levels*)))
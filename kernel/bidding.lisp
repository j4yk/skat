(in-package :skat-kernel)

(defparameter *bidding-values*
  (let ((values (append
		 (loop ; Farbspiel
		    for suit-value in '(9 10 11 12)
		    append (loop
			      for declaration-value in (loop for n from 2 to 18 collect n)
			      collect (* suit-value declaration-value)))
		 (loop with grand-value = 24 ; Grad
		    for declaration-value in (loop for n from 2 to 11 collect n)
		    collect (* grand-value declaration-value))
		 (list 23 35 46 59)))) ; Null
    (delete-duplicates (sort values #'<))))

(defun cut-away-bidding-values (bid &optional (values *bidding-values*))
  "Gibt eine Liste zurück, aus der alle Reizwerte bis vor dem angegebenen gestrichen sind.
Das erste Element der zurückgegebenen Liste ist der angegebene Reizwert."
  (if (<= bid (car values))
      values
      (cut-away-bidding-values bid (cdr values))))

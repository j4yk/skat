(in-package skat-kernel)

(defun game-variant (declaration)
  "Gibt die Spielvariante in einer Ansage zurück."
  (find-if #'(lambda (thing)
	       (find thing '(:grand :null :diamonds :hearts :spades :clubs :ramsch)))
	   declaration))
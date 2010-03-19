(in-package :skat-kernel)

(defun user-provide (host-or-player ui-implementation comm-implementation)
  "Lässt den Benutzer auf der Konsole die nicht als Programmparameter angegebenen Einstellungen auswählen."
  (values (or host-or-player 
	      (progn (format t "Are you host or player or both? ")
		     (loop for answer = (read) when (find answer '(host player both)) return it)))
	  (or ui-implementation 
	      (progn (format t "Choose an ui-implementation class name: ")
		     (loop for answer = (read) when (find-class answer nil) return it)))
	  (or comm-implementation
	      (progn (format t "Choose a comm-implementation class name: ")
		     (loop for answer = (read) when (find-class answer nil) return it)))))

(defun create-and-start (kernel-class ui-class comm-class &rest kernel-initargs)
  "Erzeugt das Spieler- oder Hostobjekt, sowie Kommunikations- und UI-Objekt und startet deren Aktivität."
  (let* ((ui (make-instance ui-class))
	 (comm (make-instance comm-class))
	 (kernel (apply 'make-instance kernel-class :ui ui :comm comm kernel-initargs)))
    (setf (ui:kernel ui) kernel)
    (comm:start comm)
    (ui:start ui)
    (ui:run ui)			   ; run the game
    ;; game has been closed, do cleanup
    (comm:stop ui)
    (comm:stop comm)))

(defun start-skat (&optional (host-or-player nil) (ui-implementation 'ui:stub-ui) (comm-implementation 'comm:stub-comm) &rest kernel-initargs)
  "Haupteintrittsfunktion des Spiels"
  (multiple-value-bind (host-or-player ui-implementation comm-implementation) 
      (user-provide host-or-player ui-implementation comm-implementation)
    (apply 'create-and-start (ecase host-or-player
			       (:player 'kern:player)
			       (:host 'kern:host))
	   ui-implementation comm-implementation
	   kernel-initargs)))

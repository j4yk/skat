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

(defun create-and-start (subject-class ui-class comm-class)
  "Erzeugt das Spieler- oder Hostobjekt, sowie Kommunikations- und UI-Objekt und startet deren Aktivität."
  (let* ((ui (make-instance ui-class))
	 (comm (make-instance comm-class))
	 (subject (make-instance 'subject-class :ui ui :comm comm)))
    (start comm)
    (start ui)))

(defun start-skat (&optional (host-or-player nil) (ui-implementation 'ui:stub-ui) (comm-implementation 'comm:stub-comm))
  "Haupteintrittsfunktion des Spiels"
  (multiple-value-bind (host-or-player ui-implementation comm-implementation) 
      (user-provide host-or-player ui-implementation comm-implementation)
    (if (eq host-or-player 'both)
	(progn
	  (sb-thread:make-thread (lambda () (create-and-start 'kern:host ui-implementation comm-implementation)) :name "Host Thread")
	  (create-and-start 'kern:player ui-implementation comm-implementation))
	(if (eq host-or-player 'player)
	    (create-and-start 'kern:player ui-implementation comm-implementation)
	    (create-and-start 'kern:host ui-implementation comm-implementation)))))

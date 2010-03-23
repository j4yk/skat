(in-package gl-ui)

(defclass error-handling (module)
  ())

(defmethod handle-error-in-ui-handler ((module error-handling) condition)
  "Pops up a text-msg error dialog that displays the condition text
and subsequently calls the last available continue restart"
  (ag:text-msg :error "In ~a trat folgender Fehler auf:~%~a"
	       (kern::handler-function-name condition)
	       (let ((*print-escape* nil))
		 (with-output-to-string (s) (print-object (kern::inner-condition condition) s))))
  (continue))

(defmethod handle-error ((module error-handling) condition)
  (ag:text-msg :error "Folgender Fehler ist Aufgetreten:~%~a~%~
Das Programm wird weiterhin ausgeführt, funktioniert jedoch~%~
unter Umständen nicht mehr wie erwartet.  Es wird empfohlen~%~
das Spiel zu schließen und neu zu starten.")
  (continue))
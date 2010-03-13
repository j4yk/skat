(in-package gl-ui)

(defclass error-handling (module)
  ())

(defmethod handle-error ((module error-handling) condition)
  "Pops up a text-msg error dialog that displays the condition text
and subsequently calls the last available continue restart"
  (ag:text-msg :error "Das Spiel wird nach folgendem Fehler fortgesetzt:~%~a"
	       (let ((*print-escape* nil))
		 (with-output-to-string (s) (print-object condition s))))
  (if (find-restart 'continue)
      (continue condition)
      (error condition)))

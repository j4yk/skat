(in-package kern)

(defun create-executable (filename toplevel-fn)
  #+sbcl (sb-ext:save-lisp-and-die filename :toplevel toplevel-fn :executable t)
  #-sbcl (error "CREATE-EXECUTABLE is only implemented for SBCL"))

(defun start-gl-ui-player ()
  (start-skat :player 'gl-ui:opengl-ui 'comm:xmpp-comm))

(defun start-host-xmpp-toplevel ()
  (format *standard-output* "Dieses Programm kann mit Strg+C beendet werden~%~%")
  (restart-case
      (handler-bind ((sb-sys:interactive-interrupt (curry #'invoke-restart 'quit))
		     (comm:login-unsuccessful
		      (lambda (condition)
			(format *standard-output* "Fehler beim Einloggen: ~{~a ~}~%"
				(if (typep (comm:additional-information condition) 'xmpp:xml-element)
				    (loop for elm in (xmpp:elements (comm:additional-information condition))
				       collect (xmpp:name elm))
				    (if (null (comm:additional-information condition))
					(list "Unbekannter Fehler")
					(list (comm:additional-information condition)))))
			(force-output *standard-output*)
			(format *standard-output* "Mit neuen Login-Daten versuchen? [J/n]: ")
			(if (member (read-char) (list #\j #\J #\Newline))
			    (invoke-restart 'retry (funcall #'comm::interactive-read-xmpp-login-data))
			    (invoke-restart 'quit)))))
	(start-host-toplevel #'comm::interactive-read-xmpp-login-data))
    (quit (&rest args) :report "Das Programm beenden" (declare (ignore args)))))

(defun create-gl-ui-xmpp-player-executable (filename)
  (create-executable filename #'start-gl-ui-player))

(defun create-host-xmpp-executable (filename)
  (create-executable filename #'start-host-xmpp-toplevel))

(in-package skat-opengl-ui)

(defclass login-and-register (module)
  ((state :accessor state :initform 'enter-login)
   (login-struct-type :accessor login-struct-type :initarg :login-struct-type)
   (login-window :accessor login-window)
   (registration-struct :accessor registration-struct)
   (register-window :accessor register-window))
  (:documentation "Modul für die Einwähl- und Registrier-Prozedur"))

(defclass agar-window ()
  ((widgets :accessor widgets)
   (window :accessor window)
   (module :accessor module :initarg :module)))

(defmethod show ((agar-window agar-window))
  "Calls AG_WindowShow on the Agar window"
  (ag:window-show (window agar-window)))

(defmethod draw ((agar-window agar-window))
  (ag:window-draw (window agar-window)))

(defmethod hide ((agar-window agar-window))
  (ag:hide-window (window agar-window)))

(defclass login-window (agar-window)
  nil)

(defmacro callback (name return-type args &body body)
  "Defines and returns the pointer to a callback function"
  `(progn
     (defcallback ,name ,return-type ,args ,@body)
     (cffi:callback ,name)))

(defmacro event-handler (function-name)
  (if (symbolp function-name)
      `(callback ,function-name :void ((event ag:event))
	 (,function-name event))
      (let ((callback-name (gensym "CALLBACK")))
	`(callback ,callback-name :void ((event ag:event))
	   (funcall ,function-name event)))))

(defmethod initialize-instance :after ((login-window login-window) &key)
  "Creates the Agar Window"
  (let ((win (agar:window-new-named "login-window" :modal :noclose)))
    (ag:window-set-caption win "Beim XMPP-Server einlogen")
    (ag:with-widgets (win
		      (ag:textbox username-textbox :label-text "Benutzername: ")
		      (ag:textbox hostname-textbox :label-text "Serveradresse: ")
		      (ag:textbox domain-textbox :label-text "Serverdomäne (optional): ")
		      (ag:textbox password-textbox :label-text "Passwort: " :flags '(:password))
		      (ag:textbox resource-textbox :label-text "Standard (optional): " :init-text "skat")
		      (ag:button login-btn "Login"))
      (ag:set-event login-btn "button-pushed" (event-handler #'(lambda (event)
								 (declare (ignore event))
								 (send-login-data (module login-window)))) "")
      (with-slots (widgets window widget-instances) login-window
	(setf window win
	      widgets (list (cons :username username-textbox)
			    (cons :hostname hostname-textbox)
			    (cons :domain domain-textbox)
			    (cons :password password-textbox)
			    (cons :resource resource-textbox)
			    (cons :login-btn login-btn)))))))

(defclass register-window (agar-window)
  nil)

(defmethod initialize-instance :after ((register-window register-window) &key)
  "Initilializes the register window by creating the widgets etc."
  (let ((win (ag:window-new-named "register-window" :modal :noclose :nominimize)))
    (ag:with-widgets (win
		      (ag:textbox host-jid-textbox :label-text "Jabber-ID des Hostes: ")
		      (ag:button register-btn "Registrierung anfragen"))
      (ag:set-event register-btn "button-pushed"
		    (event-handler #'(lambda (event)
				       (declare (ignore event))
				       (send-registration-data (module register-window))))
		    "")
      (with-slots (widgets window) register-window
	(setf window win
	      widgets (list (cons :host-jid host-jid-textbox)))))))

(defun init-login-window (module)
  "Erstelle das Login-Daten-Fenster und zeige es an."
  (setf (login-window module) (make-instance 'login-window :module module)))

(defun init-register-window (module)
  (setf (register-window module) (make-instance 'register-window :module module)))

(defmethod initialize-instance :after ((module login-and-register)
				       &key)
  "Initializes the Login window and the Registration window"
  (init-login-window module)
  (init-register-window module))

(defmethod cleanup ((module login-and-register))
  "Hides and detaches the login-window"
  (hide (login-window module))
  (hide (register-window module))
  (ag:detach-object (window (login-window module)))
  (ag:detach-object (window (register-window module))))

(defmethod query-login ((module login-and-register))
  "Shows the login-window"
  (show (login-window module)))

(defmethod query-registration ((module login-and-register))
  "Shows the register-window"
  (show (register-window module)))

(defmethod send-login-data ((module login-and-register))
  (declare (optimize debug))
  (let ((widgets (widgets (login-window module))))
    (call-kernel-handler (ui module) 'login-data
			 (comm::make-xmpp-login-data
			  :username (ag:text (cdr (assoc :username widgets)))
			  :hostname (ag:text (cdr (assoc :hostname widgets)))
			  :domain (ag:text (cdr (assoc :domain widgets)))
			  :resource (ag:text (cdr (assoc :resource widgets)))
			  :password (ag:text (cdr (assoc :password widgets)))
			  :mechanism :sasl-plain))
    (with-slots (widgets window) (login-window module)
      (let* ((wait-label (ag:label-new-string window "Bitte warten...")))
	(push (cons :wait-label wait-label) widgets)
	(ag:disable-widget (cdr (assoc :login-btn widgets)))))))

(defmethod send-registration-data ((module login-and-register))
  "Sends the registration data back to kernel and gets the data from register-window"
  (let ((widgets (widgets (register-window module))))
    (call-kernel-handler (ui module) 'registration-data
			 (comm::make-xmpp-registration-data
			  :host-jid (ag:text (cdr (assoc :host-jid widgets)))))))

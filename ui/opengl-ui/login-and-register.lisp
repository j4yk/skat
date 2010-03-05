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

(defun ensure-detached (object parent)
  (when (pointer-eq (ag:parent-object object) parent)
    (ag:detach-object object)))

(defun ensure-attached (object parent)
  (declare (optimize debug))
  (unless (pointer-eq (ag:parent-object object) parent)
    (ag:attach-object parent object)))

(defclass login-window (agar-window)
  ((username-textbox)
   (server-hostname-textbox)
   (server-domain-textbox)
   (resource-textbox)
   (password-textbox)
   (login-button)
   (wait-label)))

(defmethod initialize-instance :after ((login-window login-window) &key)
  "Creates the Agar Window"
  (let ((win (agar:window-new :modal :noclose)))
    (ag:window-set-caption win "Beim XMPP-Server einlogen")
    (ag:with-widgets (win
		      (ag:textbox username-tb :label-text "Benutzername: ")
		      (ag:textbox hostname-tb :label-text "Serveradresse: ")
		      (ag:textbox domain-tb :label-text "Serverdomäne (optional): ")
		      (ag:textbox password-tb :label-text "Passwort: " :flags '(:password))
		      (ag:textbox resource-tb :label-text "Standard (optional): " :init-text "skat")
		      (ag:button login-btn "Login"))
      (ag:set-event login-btn "button-pushed" (event-handler #'(lambda (event)
								 (declare (ignore event))
								 (send-login-data (module login-window)))) "")
      (with-slots (window username-textbox server-hostname-textbox server-domain-textbox
			  resource-textbox password-textbox login-button) login-window
	(setf window win
	      username-textbox username-tb
	      server-hostname-textbox hostname-tb
	      server-domain-textbox domain-tb
	      resource-textbox resource-tb
	      password-textbox password-tb
	      login-button login-btn)))))

(defun init-login-window (module)
  "Erstelle das Login-Daten-Fenster und zeige es an."
  (setf (login-window module) (make-instance 'login-window :module module)))

(defmethod send-login-data ((module login-and-register))
  (with-slots (window username-textbox server-hostname-textbox server-domain-textbox
		      resource-textbox password-textbox wait-label login-button)
      (login-window module)
    (call-kernel-handler (ui module) 'login-data
			 (comm::make-xmpp-login-data
			  :username (ag:text username-textbox)
			  :hostname (ag:text server-hostname-textbox)
			  :domain (ag:text server-domain-textbox)
			  :resource (ag:text resource-textbox)
			  :password (ag:text password-textbox)
			  :mechanism :sasl-plain))
    (setf wait-label (ag:label-new-string window "Bitte warten..."))
    (ag:disable-widget login-button)))

(defclass register-window (agar-window)
  ((upper-hbox)
   (failure-notice-label)
   (host-jid-textbox)
   (register-button)
   (lower-hbox)
   (wait-label)))

(defmethod initialize-instance :after ((register-window register-window) &key)
  "Initilializes the register window by creating the widgets etc."
    (with-slots (window upper-hbox failure-notice-label
			host-jid-textbox register-button lower-hbox
			wait-label)
	register-window
      (setf window (ag:window-new :modal :noclose :nominimize))
      (setf upper-hbox (ag:hbox-new window)
	    failure-notice-label
	    (ag:label-new-string (cffi:null-pointer)
				 "Host akzeptierte die Registrierungsanfrage nicht! Wählen Sie einen anderen Host.")
	    host-jid-textbox (ag:textbox-new window :label-text "Jabber-ID des Hostes: ")
	    lower-hbox (ag:hbox-new window)
	    wait-label (ag:label-new-string (null-pointer) "Warte auf Antwort des Hostes..."))
      (setf register-button (ag:button-new lower-hbox nil "Registrierung anfragen"))
      (ag:set-event register-button "button-pushed"
		    (event-handler #'(lambda (event)
				       (declare (ignore event))
				       (send-registration-data (module register-window))))
		    "")))

(defun init-register-window (module)
  (setf (register-window module) (make-instance 'register-window :module module)))

(defmethod query-registration ((register-window register-window))
  (with-slots (upper-hbox lower-hbox failure-notice-label wait-label register-button)
      register-window
    (ensure-detached failure-notice-label upper-hbox)
    (ensure-detached wait-label lower-hbox)
    (ag:enable-widget register-button)))

(defmethod send-registration-data ((module login-and-register))
  "Sends the registration data back to kernel and gets the data from register-window"
  (with-slots (window upper-hbox failure-notice-label host-jid-textbox wait-label lower-hbox register-button)
      (register-window module)
    (ensure-detached failure-notice-label upper-hbox)
    (call-kernel-handler (ui module) 'registration-data
			 (comm::make-xmpp-registration-data
			  :host-jid (ag:text host-jid-textbox)))
    (ensure-attached wait-label lower-hbox)
    (ag:disable-widget register-button)
    (ag:window-update window)))

(defmethod registration-denied ((register-window register-window))
  (with-slots (window upper-hbox failure-notice-label wait-label register-button)
      register-window
    (ag:enable-widget register-button)
    (ag:attach-object upper-hbox failure-notice-label)
    (ag:detach-object wait-label)
    (ag:window-update window)))

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
  "Hides the login-window and shows the register-window"
  (hide (login-window module))
  (query-registration (register-window module))
  (show (register-window module)))

(defmethod registration-accepted ((module login-and-register))
  "Hides the register-window"
  (hide (register-window module)))

(defmethod registration-denied ((module login-and-register))
  (registration-denied (register-window module)))
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
   (window :accessor window)))

(defmethod show ((agar-window agar-window))
  "Calls AG_WindowShow on the Agar window"
  (ag:window-show (window agar-window)))

(defmethod draw ((agar-window agar-window))
  (ag:window-draw (window agar-window)))

(defclass login-window (agar-window)
  ((module :accessor module :initarg :module)))

(defvar *login-window-widget-dispatch* (make-hash-table))

(defun register-dispatch (widget-ptr dispatch-target dispatch-table)
  (setf (gethash (cffi:pointer-address widget-ptr) dispatch-table) dispatch-target))

(cffi:defcallback login-button-handler :void ((event ag:event))
  (let ((button-ptr (ag:event-self event)))
    (if (cffi:null-pointer-p button-ptr)
	(warn "Error: didn't find Login-Button from its push callback")
	(let ((login-window (gethash (cffi:pointer-address button-ptr) *login-window-widget-dispatch*)))
	  (if login-window
	      (send-login-data (module login-window))
	      (warn "Error: didn't find login-window associated with button at ~s" button-ptr))))))

(defmethod initialize-instance :after ((login-window login-window) &key)
  "Creates the Agar Window"
  (let ((win (agar:window-new :modal)))
    (ag:window-set-caption win "Beim XMPP-Server einlogen")
    (ag:with-widgets (win
		      (ag:textbox username-textbox :label-text "Benutzername: ")
		      (ag:textbox server-textbox :label-text "Serveradresse: ")
		      (ag:textbox domain-textbox :label-text "Serverdomäne (optional): ")
		      (ag:textbox password-textbox :label-text "Passwort: " :flags '(:password))
		      (ag:textbox resource-textbox :label-text "Standard (optional): " :init-text "skat")
		      (ag:button login-btn "Login"
				 :callback-spec (ag:event-callback
						 login-button-handler
						 "")))
      (with-slots (widgets window widget-instances) login-window
	(register-dispatch login-btn login-window *login-window-widget-dispatch*)
	(setf window win
	      widgets (list (cons :username username-textbox)
			    (cons :server server-textbox)
			    (cons :domain domain-textbox)
			    (cons :password password-textbox)
			    (cons :resource resource-textbox)))))))

(defun init-login-window (module)
  "Erstelle das Login-Daten-Fenster und zeige es an."
  (setf (login-window module) (make-instance 'login-window :module module))
  (show (login-window module)))

(defmethod initialize-instance :after ((module login-and-register)
				       &key)
  (init-login-window module))

(defun hide-login-window (module)
  (ag:hide-window (login-window module)))

(defmethod draw ((module login-and-register))
  "Zeichne das Logindaten- oder Registrierungsdaten-Fenster")

(defmethod handle-event ((module login-and-register) event))

(defmethod send-login-data ((module login-and-register))
  (print 'send-login-data))

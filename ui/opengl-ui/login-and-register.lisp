(in-package skat-opengl-ui)

;; Agar windows common

(defclass agar-window ()
  ((window :accessor window)
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

(defmacro let*-slots (instance let-bindings &body body)
  "Expands into a with-slots form with the let-varnames as the slot names
and the slots being setf-ed to the supplied let-values"
  `(with-slots ,(mapcar #'car let-bindings) ,instance
     ,@(mapcar #'(lambda (let-binding-form)
		   (destructuring-bind (name val) let-binding-form
		     `(setf ,name ,val)))
	       let-bindings)
     ,@body))

(defmacro alloc-finalized (object type &rest foreign-alloc-args &key initial-element initial-contents (count 1) null-terminated-p)
  "Allocate foreign memory that is to be freed together with object"
  (declare (ignorable initial-element initial-contents count null-terminated-p))
  `(let ((ptr (foreign-alloc ,type ,@foreign-alloc-args)))
     (prog1 ptr (trivial-garbage:finalize ,object #'(lambda () (foreign-free ptr))))))

(defmacro expanded-h (widget)
  `(let ((wid ,widget))
     (prog1 wid (ag:expand-horiz wid))))

(defmacro expanded (widget)
  `(let ((wid ,widget))
     (prog1 wid (ag:expand wid))))


;; Login window

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
								 (send-login-data login-window))) "")
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

(defmethod send-login-data ((login-window login-window))
  (with-slots (window username-textbox server-hostname-textbox server-domain-textbox
		      resource-textbox password-textbox wait-label login-button)
      login-window
    (call-kernel-handler (ui (module login-window)) 'login-data
			 (comm::make-xmpp-login-data
			  :username (ag:text username-textbox)
			  :hostname (ag:text server-hostname-textbox)
			  :domain (ag:text server-domain-textbox)
			  :resource (ag:text resource-textbox)
			  :password (ag:text password-textbox)
			  :mechanism :sasl-plain))
    (setf wait-label (ag:label-new-string window "Bitte warten..."))
    (ag:disable-widget login-button)))


;; Register window

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
				       (send-registration-data register-window)))
		    "")))

(defun init-register-window (module)
  (setf (register-window module) (make-instance 'register-window :module module)))

(defmethod query-registration ((register-window register-window))
  (with-slots (upper-hbox lower-hbox failure-notice-label wait-label register-button)
      register-window
    (ensure-detached failure-notice-label upper-hbox)
    (ensure-detached wait-label lower-hbox)
    (ag:enable-widget register-button)))

(defmethod send-registration-data ((register-window register-window))
  "Sends the registration data back to kernel and gets the data from register-window"
  (with-slots (window upper-hbox failure-notice-label host-jid-textbox wait-label lower-hbox register-button)
      register-window
    (ensure-detached failure-notice-label upper-hbox)
    (call-kernel-handler (ui (module register-window)) 'registration-data
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


;; Await game start

(define-symbol-macro free-name "(unbesetzt)")

(defclass await-game-start-window (agar-window)
  ((window-vbox) (message-label)
   (player1-hbox) (player1-label) (player1-name-label)
   (player2-hbox) (player2-label) (player2-name-label)
   (start-button) (leave-button)
   (names :accessor names :type (vector string) :initform #(free-name free-name))
   (name1-fp) (name1-size) (name2-fp) (name2-size)
   (player2-name :accessor player2-name)
   (player1-name :accessor player1-name)))

(defmethod initialize-instance :after ((w await-game-start-window) &key)
  (let*-slots w
      ((name1-fp (alloc-finalized w :char :count 1024))
       (name1-size 1024)
       (name2-fp (alloc-finalized w :char :count 1024))
       (name2-size 1024)
       (window (ag:window-new :noclose))
       (window-vbox (expanded (ag:vbox-new window)))
       (message-label (ag:label-new-string window-vbox "Warte auf Mitspieler..."))
       (player1-label (expanded-h (ag:new-polled-label window-vbox nil "1. Mitspieler: %s" name1-fp)))
       (player2-label (expanded-h (ag:new-polled-label window-vbox nil "2. Mitspieler: %s" name2-fp)))
       (start-button (expanded-h (let ((btn (ag:button-new window-vbox nil "Starte die Runde")))
				   (prog1 btn (ag:disable-widget btn)))))
       (leave-button (expanded-h (ag:button-new window-vbox nil "Die Runde verlassen"))))
    (ag:window-set-caption window "Mitspieler")
    (ag:set-event start-button "button-pushed" (event-handler #'(lambda (event)
								  (declare (ignore event))
								  (request-game-start w))) "")
    (setf (player1-name w) free-name)
    (setf (player2-name w) free-name)))

(defmethod (setf player1-name) :after ((name string) (w await-game-start-window))
  "Updates the foreign string that is polled by player1-label"
  (with-slots (name1-fp name1-size) w
    (lisp-string-to-foreign name name1-fp name1-size)))

(defmethod (setf player2-name) :after ((name string) (w await-game-start-window))
  "Updates the foreign string that is polled by player2-label"
  (with-slots (name2-fp name2-size) w
    (lisp-string-to-foreign name name2-fp name2-size)))

(defmethod enable-start-button ((w await-game-start-window))
  (with-slots (start-button) w
    (ag:enable-widget start-button)))

(defmethod disable-start-button ((w await-game-start-window))
  (with-slots (start-button) w
    (ag:disable-widget start-button)))

(defmethod add-player (name (w await-game-start-window))
  "Fügt einen Mitspieler zur Liste hinzu"
  (if (string= (aref (names w) 0) free-name)
      (setf (aref (names w) 0) name
	    (player1-name w) name)
      (if (string= (aref (names w) 1) free-name)
	  (prog1
	      (setf (aref (names w) 1) name
		    (player2-name w) name)
	    ;; wenn der zweite Spieler dazukam, kann das Spiel gestartet werden
	    (enable-start-button w))
	  (error "Only two players can be added to await-game-start-window"))))

(defmethod remove-player (name (w await-game-start-window))
  "Streicht einen Mitspieler aus der Liste und lässt ggf. den vorher zweiten Mitspieler aufrutschen"
  (if (string= (aref (names w) 0) name)
      ;; lösche ersten Namen und rücke den zweiten auf
      (progn
	(setf (aref (names w) 0) (aref (names w) 1)
	      (aref (names w) 1) free-name
	      (player1-name w) (player2-name w)
	      (player2-name w) free-name)
	(disable-start-button w))
      (if (string= (aref (names w) 1) name)
	  ;; lösche zweiten Namen
	  (progn
	    (setf (aref (names w) 1) free-name
		  (player2-name w) free-name)
	    (disable-start-button w))
	  (warn "~s: ~a was not in the list of possible participators and thus has not been deleted"
		(class-name (class-of w)) name))))

(defmethod request-game-start ((w await-game-start-window))
  "Sends kernel a game-start request"
  (call-kernel-handler (ui (module w)) 'game-start))


;; Module

(defclass login-and-register (module)
  ((state :accessor state :initform 'enter-login)
   (login-struct-type :accessor login-struct-type :initarg :login-struct-type)
   (login-window :accessor login-window)
   (registration-struct :accessor registration-struct)
   (register-window :accessor register-window)
   (await-game-start-window :accessor await-game-start-window))
  (:documentation "Modul für die Einwähl- und Registrier-Prozedur"))

(defmethod initialize-instance :after ((module login-and-register)
				       &key)
  "Initializes the Login window and the Registration window"
  (init-login-window module)
  (init-register-window module)
  (setf (await-game-start-window module) (make-instance 'await-game-start-window :module module)))

(defmethod cleanup ((module login-and-register))
  "Hides and detaches the login-window"
  (hide (login-window module))
  (hide (register-window module))
  (hide (await-game-start-window module))
  (ag:detach-object (window (login-window module)))
  (ag:detach-object (window (register-window module)))
  (ag:detach-object (window (await-game-start-window module))))

(defmethod query-login ((module login-and-register))
  "Shows the login-window"
  (show (login-window module)))

(defmethod query-registration ((module login-and-register))
  "Hides the login-window and shows the register-window"
  (hide (login-window module))
  (query-registration (register-window module))
  (show (register-window module)))

(defmethod registration-accepted ((module login-and-register))
  "Hides the register-window and shows await-game-start-window instead"
  (hide (register-window module))
  (show (await-game-start-window module)))

(defmethod registration-denied ((module login-and-register))
  (registration-denied (register-window module)))

(defmethod player-joined ((module login-and-register) player-name)
  (add-player player-name (await-game-start-window module)))

(defmethod player-left ((module login-and-register) player-name)
  (remove-player player-name (await-game-start-window module)))

(defmethod game-starts ((module login-and-register))
  "Hides all login-and-register's windows"
  (map nil #'hide (list (login-window module)
			(register-window module)
			(await-game-start-window module))))

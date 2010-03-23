(in-package skat-opengl-ui)

;; Login window

(defclass login-window (agar-window)
  ((username-textbox)
   (server-hostname-textbox)
   (server-domain-textbox)
   (resource-textbox)
   (password-textbox)
   (login-button)
   (wait-label)
   (wait-timeout)
   (timeout-label)
   (cancel-button)))

(defmethod initialize-instance :after ((login-window login-window) &key)
  "Creates the Agar Window"
  (let*-slots login-window
      ((window (agar:window-new :modal :noclose))
       (username-textbox (ag:textbox-new window :label-text "Benutzername: "))
       (server-hostname-textbox (ag:textbox-new window :label-text "Serveraddresse: "))
       (server-domain-textbox (ag:textbox-new window :label-text "Serverdomäne (optional): "))
       (password-textbox (ag:textbox-new window :label-text "Passwort: " :flags '(:password)))
       (resource-textbox (ag:textbox-new window :label-text "Ressource (optional): " :init-text "skat"))
       (login-button (expanded-h (ag:new-button window nil "Einloggen"
						(std-event-handler (send-login-data
						login-window)))))
       (wait-timeout (alloc-finalized login-window 'ag:timeout))
       (wait-label (ag:new-label (null-pointer) nil "Bitte warten, verbinde..."))
       (timeout-label (expanded-h (ag:new-label (null-pointer) nil
						(format nil "Anscheinend braucht der Server~%lange für eine Antwort"))))
       (cancel-button (expanded-h (ag:new-button (null-pointer) nil "Abbrechen"
						 (std-event-handler (cancel-login
						 login-window))))))
    (ag:window-set-caption window "Beim XMPP-Server einlogen")
    ;; initialize the timeout for login
    (ag:set-timeout wait-timeout (lambda-timeout-callback (obj ival arg)
				   (declare (ignore obj ival arg))
				   (login-timed-out login-window)
				   0) ; don't reschedule
		    (null-pointer) nil)))

(defmethod cancel-login ((login-window login-window))
  "Hide the wait- and timeout-label and the cancel-button,
enable the login-button and autosize the window"
  (with-slots (wait-label timeout-label cancel-button login-button window) login-window
    (mapcar (rcurry #'ensure-detached window)
	    (list wait-label timeout-label cancel-button))
    (ag:enable-widget login-button)
    (autosize login-window)))

(defmethod show :before ((login-window login-window))
  "Make sure the wait- and timeout-labels are detached"
  ;; this is just like cancelling the login
  (cancel-login login-window))

(defmethod send-login-data ((login-window login-window))
  (with-slots (window username-textbox server-hostname-textbox server-domain-textbox
		      resource-textbox password-textbox wait-label login-button
		      wait-timeout)
      login-window
    (restart-case
	(handler-bind ((comm:login-unsuccessful (curry #'invoke-restart 'continue)))
	  (progn
	    (ensure-attached wait-label window)
	    ;; start the timer
	    (ag:schedule-timeout (null-pointer) wait-timeout (* 10 1000)) ; 10 Sekunden
	    (autosize login-window)
	    (ag:disable-widget login-button)
	    (call-kernel-handler (ui (module login-window)) 'login-data
				 (comm::make-xmpp-login-data
				  :username (ag:text username-textbox)
				  :hostname (ag:text server-hostname-textbox)
				  :domain (ag:text server-domain-textbox)
				  :resource (ag:text resource-textbox)
				  :password (ag:text password-textbox)
				  :mechanism :sasl-plain))))
      (continue (&optional condition)
	:report "Show Login error and proceed"
	(ag:delete-timeout (null-pointer) wait-timeout)
	(ag:text-msg
	 :error "Fehler beim Einloggen: ~{~a ~}"
	 (if condition
	     (typecase (comm:additional-information condition)
	       (xmpp:xml-element
		(let ((failure-element (comm:additional-information condition)))
		  (loop for elm in (xmpp:elements failure-element)
		     collect (xmpp:name elm))))
	       (t (list "Unbekannter Fehler")))
	     (list "Unbekannter Fehler")))
	(ensure-detached wait-label window)
	(autosize login-window)
	(ag:enable-widget login-button)))))


(defmethod login-timed-out ((login-window login-window))
  (with-slots (timeout-label cancel-button window) login-window
    (ensure-attached timeout-label window)
    (ensure-attached cancel-button window)
    (autosize login-window)))

(defmethod hide :before ((login-window login-window))
  "Deletes the wait timeout before the windows is hidden."
  (with-slots (wait-timeout) login-window
    (ag:delete-timeout (null-pointer) wait-timeout)))


;; Register window

(defclass register-window (agar-window)
  ((upper-hbox)
   (failure-notice-label)
   (host-jid-textbox)
   (register-button)
   (lower-hbox)
   (wait-label)
   (wait-timeout)
   (timeout-label)
   (cancel-button)))

(defmethod initialize-instance :after ((register-window register-window) &key)
  "Initilializes the register window by creating the widgets etc."
  (let*-slots register-window
      ((window (ag:window-new :modal :noclose :nominimize))
       (upper-hbox (expanded-h (ag:hbox-new window)))
       (failure-notice-label
	(expanded-h (ag:new-label (cffi:null-pointer) nil
				  (format nil "Host akzeptierte ~
die Registrierungsanfrage nicht!~% Wählen Sie einen anderen Host."))))
       (host-jid-textbox (ag:textbox-new window :label-text "Jabber-ID des Hostes: "))
       (lower-hbox (expanded-h (ag:hbox-new window)))
       (wait-label (ag:new-label (null-pointer) nil "Warte auf Antwort des Hostes..."))
       (register-button (ag:new-button lower-hbox nil "Registrierung anfragen"
				       (std-event-handler
					 (send-registration-data register-window))))
       (wait-timeout (alloc-finalized register-window 'ag:timeout))
       (timeout-label (ag:new-label (null-pointer) nil "Der Host braucht lange für eine Antwort"))
       (cancel-button (ag:new-button (null-pointer) nil "Abbrechen"
				     (std-event-handler
				       (cancel-registration register-window)))))
    (ag:window-set-caption window "Registrierung mit einem Host")
    (ag:set-timeout wait-timeout (lambda-timeout-callback (obj ival arg)
				   (declare (ignore obj ival arg))
				   (registration-timeout register-window)
				   0)
		    (null-pointer) nil)))

(defmethod cancel-registration ((register-window register-window))
  "Detach wait-, and timeout-label and cancel-button,
enable register-button and autosize the window"
  (with-slots (window register-button lower-hbox wait-label timeout-label cancel-button)
      register-window
    (mapcar (rcurry #'ensure-detached window)
	    (list timeout-label cancel-button))
    (ensure-detached wait-label lower-hbox)
    (ag:enable-widget register-button)
    (autosize register-window)))

(defmethod show :before ((register-window register-window))
  (with-slots (upper-hbox lower-hbox failure-notice-label wait-label register-button)
      register-window
    ;; equal to cancelling registration
    (cancel-registration register-window)
    ;; plus detaching the failure notice
    (ensure-detached failure-notice-label upper-hbox)))

(defmethod send-registration-data ((register-window register-window))
  "Sends the registration data back to kernel and gets the data from register-window"
  (with-slots (window upper-hbox failure-notice-label host-jid-textbox wait-label lower-hbox register-button
		      wait-timeout)
      register-window
    (ensure-detached failure-notice-label upper-hbox)
    (call-kernel-handler (ui (module register-window)) 'registration-data
			 (comm::make-xmpp-registration-data
			  :host-jid (ag:text host-jid-textbox)))
    ;; start timer
    (ag:schedule-timeout (null-pointer) wait-timeout (* 10 1000))
    ;; show wait label
    (ensure-attached wait-label lower-hbox)
    (ag:disable-widget register-button)
    (autosize register-window)))

(defmethod registration-timeout ((register-window register-window))
  (with-slots (window timeout-label cancel-button) register-window
    (ensure-attached timeout-label window)
    (ensure-attached cancel-button window)
    (autosize register-window)))

(defmethod registration-denied ((register-window register-window))
  "Show failure notice and cancel registration"
  (with-slots (window upper-hbox failure-notice-label)
      register-window
    (ensure-attached failure-notice-label upper-hbox)
    (cancel-registration register-window)))

(defmethod hide :after ((register-window register-window))
  "Deletes the wait timeout before hiding the window"
  (with-slots (wait-timeout) register-window
    (ag:delete-timeout (null-pointer) wait-timeout)))


;; Await game start

(define-symbol-macro free-name "(unbesetzt)")

(defclass await-game-start-window (agar-window)
  ((window-vbox) (message-label)
   (player1-hbox) (player1-label) (player1-name-label)
   (player2-hbox) (player2-label) (player2-name-label)
   (start-button) (leave-button)
   (names :accessor names :type (vector string) :initform (make-array 2 :initial-element nil))
   (name1-fp) (name1-size) (name2-fp) (name2-size)
   (player2-name :accessor player2-name)
   (player1-name :accessor player1-name)
   (wait-label)))

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
       (start-button (expanded-h (let ((btn (ag:new-button window-vbox nil "Starte die Runde"
							   (std-event-handler (request-game-start w)))))
				   (prog1 btn (ag:disable-widget btn)))))
       (leave-button (expanded-h (ag:new-button window-vbox nil "Die Runde verlassen"
						(std-event-handler (leave w)))))
       (wait-label (expanded-h (ag:new-label (null-pointer) nil
					     (format nil "Warte darauf, dass~%die Mitspieler auch starten")))))
    (ag:window-set-caption window "Mitspieler")
    (setf (player1-name w) free-name)
    (setf (player2-name w) free-name)))

(defmethod show :before ((w await-game-start-window))
  "Remove any players that are there from a previous lobby
and makes sure the wait-label is detached"
  (loop until (null (aref (names w) 0))
       do (remove-player (aref (names w) 0) w))
  (with-slots (window-vbox wait-label) w
    (ensure-detached wait-label window-vbox)))

(defmethod (setf player1-name) :after ((name string) (w await-game-start-window))
  "Updates the foreign string that is polled by player1-label"
  (with-slots (name1-fp name1-size) w
    (lisp-string-to-foreign name name1-fp name1-size))
  (autosize w))

(defmethod (setf player2-name) :after ((name string) (w await-game-start-window))
  "Updates the foreign string that is polled by player2-label"
  (with-slots (name2-fp name2-size) w
    (lisp-string-to-foreign name name2-fp name2-size))
  (autosize w))

(defmethod enable-start-button ((w await-game-start-window))
  (with-slots (start-button) w
    (ag:enable-widget start-button)))

(defmethod disable-start-button ((w await-game-start-window))
  (with-slots (start-button) w
    (ag:disable-widget start-button)))

(defmethod add-player (name (w await-game-start-window))
  "Fügt einen Mitspieler zur Liste hinzu"
  (if (null (aref (names w) 0))
      (setf (aref (names w) 0) name
	    (player1-name w) name)
      (if (null (aref (names w) 1))
	  (prog1
	      (setf (aref (names w) 1) name
		    (player2-name w) name)
	    ;; wenn der zweite Spieler dazukam, kann das Spiel gestartet werden
	    (enable-start-button w))
	  (error "Only two players can be added to await-game-start-window"))))

(defmethod remove-player (name (w await-game-start-window))
  "Streicht einen Mitspieler aus der Liste und lässt ggf. den vorher zweiten Mitspieler aufrutschen"
  (if (equal (aref (names w) 0) name)
      ;; lösche ersten Namen und rücke den zweiten auf
      (progn
	(setf (aref (names w) 0) (aref (names w) 1)
	      (aref (names w) 1) nil
	      (player1-name w) (player2-name w)
	      (player2-name w) free-name)
	(disable-start-button w))
      (if (string= (aref (names w) 1) name)
	  ;; lösche zweiten Namen
	  (progn
	    (setf (aref (names w) 1) nil
		  (player2-name w) free-name)
	    (disable-start-button w))
	  (warn "~s: ~a was not in the list of possible participators and thus has not been deleted"
		(class-name (class-of w)) name))))

(defmethod request-game-start ((w await-game-start-window))
  "Sends kernel a game-start request"
  (call-kernel-handler (ui (module w)) 'game-start)
  (with-slots (window-vbox wait-label) w
    (ensure-attached wait-label window-vbox)))

(defmethod leave ((w await-game-start-window))
  "Calls leave on login-and-register module and hides await-game-start-window"
  (hide w)
  (leave (module w)))


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
  (setf (login-window module) (make-instance 'login-window :module module)
	(register-window module) (make-instance 'register-window :module module)
	(await-game-start-window module) (make-instance 'await-game-start-window :module module)))

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

(defmethod leave ((module login-and-register))
  "Sends unregister to kernel and redisplays the register-window"
  (call-kernel-handler (ui module) 'unregister)
  (show (register-window module)))

(defmethod game-starts ((module login-and-register))
  "Hides all login-and-register's windows"
  (map nil #'hide (list (login-window module)
			(register-window module)
			(await-game-start-window module))))

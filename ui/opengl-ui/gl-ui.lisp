(in-package gl-ui)

(defclass opengl-ui (ui::base-ui)
  ((modules :accessor modules :initform nil :documentation "Liste der aktiven Module")
   (priortized-modules :accessor priortized-modules :initform nil :documentation "Liste der aktiven Module, die Events zuerst verarbeiten dürfen")
   (running-p :reader running-p :initform nil))
  (:documentation "OpenGL-UI Klasse. Zentrales Objekt für die OpenGL-Schnittstelle"))

(defun find-module (class ui)
  "Returns the first module of given class from (modules ui)"
  (find (find-class class) (modules ui) :key #'class-of))

(defun insert-module (module ui)
  (push module (modules ui)))

(defun remove-module (module ui)
  (setf (modules ui) (delete module (modules ui))))

(defun cleanup-and-remove-module (module ui)
  (cleanup module)
  (remove-module module ui))

(defmethod ui:start ((ui opengl-ui) &optional no-new-thread-p)
  "Startet die OpenGL-Benutzerschnittstelle.
STUB"
  (declare (ignore ui no-new-thread-p))
  (error "Not implemented yet!"))

(defmethod ui:stop ((ui opengl-ui))
  "Hält die OpenGL-Benutzerschnittstelle an.
STUB"
  (declare (ignore ui))
  (error "Not implemented yet!"))

(defun skat-window ()
  "Erstellt das Skat-SDL-Fenster inklusive OpenGL-Kontext."
  (prog1 (sdl:window 640 480 :title-caption "Skat"
		     :fps (make-instance 'sdl:fps-timestep)
		     :flags '(sdl:sdl-opengl sdl:sdl-doublebuf))
    (init-gl 640 480)))

(defhandler ui:login-struct (opengl-ui struct-classname)
  (let ((module (find-module 'login-and-register ui)))
    (query-login module)))

;; zu Senden: Login-Data

(defhandler ui:registration-struct (opengl-ui struct-classname)
  (let ((module (find-module 'login-and-register ui)))
    (query-registration module)))

;; zu Senden: Registration-Data

(defhandler ui:registration-reply (opengl-ui accepted)
  (let ((module (find-module 'login-and-register ui)))
    (if accepted
	(registration-accepted module)
	(registration-denied module))))

(defhandler ui:server-update (opengl-ui events)
  #| idle |#)

;; zu Senden: Unregister

(defhandler ui:logout (opengl-ui address)
  #| idle |#)

(defhandler ui:playmates (opengl-ui left right)
  (let ((module (find-module 'players ui)))
    (playmates module left right)))

;; zu Senden: Game-Start

(defhandler ui:game-start (opengl-ui)
  #| keine Ahnung |#)

(defhandler ui:cards (opengl-ui cards)
  "Called by Kernel when the Host has distrubuted the cards.
Hands the cards over to the cards module."
  (let ((cards-mod (find-module 'cards ui)))
    (unless cards-mod
      (let ((mod (make-instance 'cards :ui ui)))
	(insert-module mod ui)
	(setf cards-mod mod)))
    (setf (cards cards-mod) cards)))

(defhandler ui:start-bidding (opengl-ui listener min-value)
  (let ((module (find-module 'bidding ui)))
    (start-bidding module listener min-value)))

(defhandler ui:listen (opengl-ui bidder)
  (let ((module (find-module 'bidding ui)))
    (new-listener module bidder)))

(defhandler ui:bid (opengl-ui value)
  (let ((module (find-module 'bidding ui)))
    (bid-received module sender value)))

(defhandler ui:reply-to-bid (opengl-ui value)
  (let ((module (find-module 'bidding ui)))
    (query-join module value)))

(defhandler ui:pass (opengl-ui value)
  (let ((module (find-module 'bidding ui)))
    (player-passed module sender value)))

(defmethod send-skat ((ui opengl-ui) skat)
  "Sends the chosen cards back to Kernel and removes the cards from
  the player's hand"
  (unless (= 2 (length skat)) (error "Must push two cards into the skat!"))
  (let ((cards-mod (find-module 'cards ui)))
    (remove-cards cards-mod skat)
    (end-choose-skat cards-mod)		; cleanup
    (ui:call-kernel-handler ui 'skat skat)) ; pass it on
  (remove-module (find-module 'send-button ui) ui))

(defhandler ui:skat (opengl-ui skat)
  "Called by Kernel when the skat is received from the host.
Adds two cards and lets the player select two."
  (assert (= 2 (length skat)) () "Skat must be two cards")
  (let ((cards-mod (find-module 'cards ui)))
    (assert cards-mod)
    (setf (cards cards-mod) (nconc (cards cards-mod) skat))
    ;; send button
    (insert-module (make-instance 'send-button
				  :ui ui
				  :handler-fn #'(lambda ()
						  (send-skat ui (selected-cards cards-mod))))
		   ui)))

(defmethod render-everything ((ui opengl-ui) agar-module)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (if ag:*video-initialized*
      (ag:render
	(with-standard-rendering
	  ;; draw all modules with normal GL matrices
	  (dolist (module (remove agar-module (modules ui)))
	    (draw module)))
	;; draw Agar's world with its own matrices
	(draw agar-module))
      (dolist (module (remove agar-module (modules ui)))
	(draw module))))

(defun standard-main-loop (ui)
  (when (running-p ui) (return-from standard-main-loop))
  (setf (slot-value ui 'running-p) t)
  (unwind-protect
       (let ((agar-module (find-module 'agar ui)))
	 (defun %used-agar-module () agar-module)
	 (macrolet ((process-event ()
		      `(dolist (module (modules ui))
			 (handle-event module sdl-event))))
	   (labels ((main-loop ()
		      (sdl:with-events (:poll sdl-event)
			(:quit-event () t)
			(:active-event () (process-event))
			(:key-down-event () (process-event))
			(:key-up-event () (process-event))
			(:mouse-motion-event () (process-event))
			(:mouse-button-down-event () (process-event))
			(:mouse-button-up-event () (process-event))
			(:video-resize-event (:w w :h h)
					     (init-gl w h)
					     (process-event))
			(:video-expose-event () (process-event))
			(:sys-wm-event () (process-event))
			(:user-event () (process-event))
			(:idle ()
			       (handle-swank-requests)
			       (render-everything ui agar-module)
			       (gl:flush)
			       (sdl:update-display))))
		    (continuable-main-loop ()
		      (restart-case (main-loop)
			(continue-main-loop () (continuable-main-loop)))))
	     (continuable-main-loop))))
    (setf (slot-value ui 'running-p) nil)))

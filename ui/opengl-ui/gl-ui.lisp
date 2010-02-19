(in-package gl-ui)

(defclass opengl-ui (ui::base-ui)
  ((modules :accessor modules :initform nil :documentation "Liste der aktiven Module")
   (priortized-modules :accessor priortized-modules :initform nil :documentation "Liste der aktiven Module, die Events zuerst verarbeiten dürfen"))
  (:documentation "OpenGL-UI Klasse. Zentrales Objekt für die OpenGL-Schnittstelle"))

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

(defvar *modelview-root-matrix* nil "Saved modelview matrix")
(defvar *texture-root-matrix* nil "Saved texture matrix")
(defvar *projection-root-matrix* nil "Saved projection matrix")

(defun set-perspective (w h)
  (with-matrix-mode :projection
    (glu:perspective 60.0 (/ w h) 0.1 1024.0)))

(defun init-gl (w h)
  (gl:clear-color 0 0 0 0)
  (gl:shade-model :smooth)
  ;; Perspektive
  (with-matrix-mode :projection
    (gl:load-identity)
    (set-perspective w h))
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  ;; save matrices
  (setq *modelview-root-matrix* (gl:get-integer :modelview-matrix)
	*texture-root-matrix* (gl:get-integer :texture-matrix)
	*projection-root-matrix* (gl:get-integer :projection-matrix)))

(defun skat-window ()
  "Erstellt das Skat-SDL-Fenster inklusive OpenGL-Kontext."
  (sdl:window 640 480 :title-caption "Skat"
	      :flags '(sdl:sdl-opengl sdl:sdl-doublebuf)))

(defmacro restartable (&body body)
  `(restart-case (progn ,@body)
     (continue () t)))

(defun handle-swank-requests ()
  (restartable			; SLIME-Sachen ausführen
   (let ((connection
	  (or swank::*emacs-connection* (swank::default-connection))))
     (when (and connection) ;(not (eql swank:*communication-style* :spawn)))
       (swank::handle-requests connection t)))))

(defmacro case-event (sdl-event &body events)
  "Führt verschiedene Blöcke aus, abhängig vom Typ des Events.
Syntax entspricht der von sdl:with-events und bedient sich bei
Lispbuilders Funktionen."
  ;; Basiert auf sdl:with-events Code
  `(cond
     ,@(remove nil (mapcar #'(lambda (event)
			       (when (gethash (first event) sdl::*events*)
				 (sdl::expand-event sdl-event
						    (first event) ; Event-Type (z. B. :mouse-motion-event)
						    (gethash (first event) sdl::*events*)
						    (first (rest event)) ; Parameters
						    (rest (rest event))))) ; Handler-Body
			   events))))

#+agar
(defhandler ui:login-struct (opengl-ui struct-classname)
  (let ((module (make-instance 'login-and-register-module :login-struct-type struct-classname)))
    (push module (modules ui))))

(defmacro with-root-matrices ((&key (modelview-p t) (projection-p t) (texture-p t)) &body body)
  `(progn
     ,(when modelview-p
	    `(with-matrix-mode :modelview (gl:push-matrix)
		    (gl:load-matrix *modelview-root-matrix*)))
     ,(when projection-p
	    `(with-matrix-mode :projection (gl:push-matrix)
		    (gl:load-matrix *projection-root-matrix*)))
     ,(when texture-p
	    `(with-matrix-mode :texture (gl:push-matrix)
		    (gl:load-matrix *texture-root-matrix*)))
     ,@body
     ,(when modelview-p
	    `(progn (gl:matrix-mode :modelview) (gl:pop-matrix)))
     ,(when projection-p
	    `(progn (gl:matrix-mode :projection) (gl:pop-matrix)))
     ,(when texture-p
	    `(progn (gl:matrix-mode :texture) (gl:pop-matrix)))))
     

#+agar
(defmacro non-agar-rendering (&body body)
  `(progn
     ;; restore our own matrices
     (with-root-matrices ()
       (gl:load-identity)
       ;; restore our attributes
       (gl:push-attrib :all-attrib-bits)
       (gl:disable :clip-plane0 :clip-plane1 :clip-plane2 :clip-plane3 :clip-plane4 :clip-plane5)
       (gl:enable :cull-face)
       
       ,@body

       ;; restore Agar's attributes
       (gl:pop-attrib))))

#-agar
(defmacro non-agar-rendering (&body body)
  `(progn ,@body))

(defun standard-main-loop (ui)
  (sdl:with-events (:poll sdl-event)
    (:quit-event () t)
    (:video-resize-event (:w w :h h)
			 (init-gl w h)
			 (dolist (module (modules ui))
			   (handle-event module sdl-event)))
    (:mouse-button-down-event ()
			      (dolist (module (modules ui))
				(handle-event module sdl-event)))
    (:mouse-button-up-event ()
			    (dolist (module (modules ui))
			      (handle-event module sdl-event)))
    (:mouse-motion-event ()
			 (dolist (module (modules ui))
			   (handle-event module sdl-event)))
    (:key-down-event ()
		     (dolist (module (modules ui))
		       (handle-event module sdl-event)))
    (:key-up-event ()
		   (dolist (module (modules ui))
		     (handle-event module sdl-event)))
    (:idle ()
	   (handle-swank-requests)
	   (gl:clear :color-buffer-bit :depth-buffer-bit)
	   (gl:enable :depth-test)
	   ;; reset view
	   #+agar
	   (if ag::*video-initialized*
	       ;; with agar
	       (ag:render
		 (non-agar-rendering
		   (dolist (module (modules ui))
		     (draw module)))
		 (let ((win (ag::tailqueue-first (ag::windows ag::*view*))))
		   ;; TODO: render the other windows as well...
		   (unless (cffi:null-pointer-p win)
		     (ag:window-draw win))))
	       ;; without agar
	       (dolist (module (modules ui))
		 (draw module)))
	   #-agar
	   (dolist (module (modules ui))
	     (draw module))
	   (gl:flush)
	   (sdl:update-display))))

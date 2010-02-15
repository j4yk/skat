(in-package gl-ui)

(defmacro restartable (&body body)
  `(restart-case (progn ,@body)
     (continue () t)))

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

(defclass test-module (module)
  ((textures-updated-p :accessor textures-updated-p :initform nil)
   (texture :accessor texture)
   (blue-tex :accessor blue-tex)))

(defun init-gl ()
  (gl:clear-color 0 1 0 0)
  ;; Perspektive
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:perspective 60.0 1.0 0.1 1024.0)
  (gl:matrix-mode :modelview))

(defmethod draw ((module test-module))
  (update-textures module)			; wenn es neues in dieser Funktion gibt, führe das aus
  (gl:enable :texture-2d)
;;  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gl:load-identity)
  (gl:translate 0 0 -10)
  (gl:bind-texture :texture-2d (blue-tex module))
  (gl:matrix-mode :texture)
  (gl:load-identity)			; wahrscheinlich braucht man das gar nicht immer
  (gl:matrix-mode :modelview)
  ;; Blaues Dreieck
  (gl:color 1 1 1)
  (gl:with-primitive :triangles
    (gl:tex-coord 0 1) (gl:vertex 0 2 -1)
    (gl:tex-coord 0 0) (gl:vertex 0 0 -1)
    (gl:tex-coord 1 0) (gl:vertex 1 0 -2))
  (gl:bind-texture :texture-2d (texture module))
  (gl:matrix-mode :texture)
  (gl:load-identity)			; siehe oben
  (gl:matrix-mode :modelview)
  ;; verzerrte Testkarte
  (gl:color 1 1 1)
  (gl:with-primitive :quads
    (gl:tex-coord 0 1) (gl:vertex -3 -3)
    (gl:tex-coord 1 1) (gl:vertex 5 -3)
    (gl:tex-coord 1 0) (gl:vertex 5 0)
    (gl:tex-coord 0 0) (gl:vertex -3 0))
  ;; zeigen
  ;; (gl:flush)
  ;; (sdl:update-display)
  (gl:disable :texture-2d))		; braucht man bestimmt auch nicht

(progn
  (eval-when (:compile-toplevel)
    (defparameter *textures-updated* nil))
  (defmethod update-textures ((module test-module))
    (unless (and *textures-updated* (textures-updated-p module))
      (setf *textures-updated* t
	    (textures-updated-p module) t)
      (format t "~&Updating textures~%")
      (with-slots (texture blue-tex) module
	(free-textures module)
	;; eine per Hand erstellte Textur... blaue Fläche
	(setf blue-tex (sdl:with-surface (s (sdl:create-surface 512 512))
			 (sdl:fill-surface-* 0 0 255 :clipping nil)
			 ;; mit clipping gibts eine Exception wegen Null-Pointer, 
			 ;; da der hier nicht 
			 (sdl-surface-to-gl-texture s :rgba)))
	;; und eine aus dem Bild
	(setf texture (texture-from-bmp "diamonds7.bmp")))
      (break "~a ~a" (texture module) (blue-tex module)))))

(defmethod free-textures ((mod test-module))
  (gl:delete-textures (loop for slot in (list 'texture 'blue-tex)
			 when (slot-boundp mod slot)
			 collect (prog1
				     (slot-value mod slot)
				   (slot-makunbound mod slot)))))

(defun skat-window ()
  "Erstellt das Skat-SDL-Fenster inklusive OpenGL-Kontext."
  (sdl:window 640 480 :title-caption "Skat"
	      :flags '(sdl:sdl-opengl sdl:sdl-doublebuf)))

(defun handle-swank-requests ()
  (restartable			; SLIME-Sachen ausführen
   (let ((connection
	  (or swank::*emacs-connection* (swank::default-connection))))
     (when (and connection (not (eql swank:*communication-style* :spawn)))
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

(defhandler ui:login-struct (opengl-ui struct-classname)
  (let ((module (make-instance 'login-and-register-module :login-struct-type struct-classname)))
    (push module (modules ui))))

(defun simple-sdl-test ()
  (let ((ui (make-instance 'opengl-ui))
	(testm (make-instance 'test-module)))
    (push testm (modules ui))
    (sdl:with-init ()
      (skat-window)
      (setf (sdl:frame-rate) 2)
      (init-gl)
      (sdl:show-cursor :enable)		; mit Cursor bitte
      (update-textures testm)
      (standard-main-loop ui)
      (free-textures testm))))

(defun standard-main-loop (ui)
  (sdl:with-events (:poll sdl-event)
    (:quit-event () t)
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
	   ;; reset view
	   (gl:matrix-mode :modelview)
	   (gl:load-identity)
	   (ag:render			; begin rendering routine for Agar-Modules also
	     (dolist (module (modules ui))
	       (draw module)))
	   (gl:flush)
	   (sdl:update-display))))

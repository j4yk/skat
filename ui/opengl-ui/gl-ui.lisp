(in-package gl-ui)

(defclass opengl-ui (ui::base-ui)
  ((modules :accessor modules :initform nil :documentation "Liste der aktiven Module")
   (priortized-modules :accessor priortized-modules :initform nil :documentation "Liste der aktiven Module, die Events zuerst verarbeiten dürfen"))
  (:documentation "OpenGL-UI Klasse. Zentrales Objekt für die OpenGL-Schnittstelle"))

(defun find-module (class ui)
  "Returns the first module of given class from (modules ui)"
  (find (find-class class) (modules ui) :key #'class-of))

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
		     :flags '(sdl:sdl-opengl sdl:sdl-doublebuf))
    (init-gl 640 480)))

#+agar
(defhandler ui:login-struct (opengl-ui struct-classname)
  (let ((module (make-instance 'login-and-register-module :login-struct-type struct-classname)))
    (push module (modules ui))))

(defun standard-main-loop (ui)
  (let ((agar-module (find (find-class 'agar) (modules ui) :key #'class-of)))
    (macrolet ((process-event ()
		 `(dolist (module (modules ui))
		    (handle-event module sdl-event))))
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
	       (gl:clear :color-buffer-bit :depth-buffer-bit)
	       ;; reset view
	       #+agar
	       (if ag::*video-initialized*
		   (ag:render
		     (non-agar-rendering
		       (dolist (module (remove agar-module (modules ui)))
			 (draw module)))
		     (draw agar-module))
		   (dolist (module (remove agar-module (modules ui)))
		     (draw module)))
	       #-agar
	       (dolist (module (modules ui))
		 (draw module))
	       (gl:flush)
	       (sdl:update-display))))))

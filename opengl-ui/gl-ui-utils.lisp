(in-package gl-ui)

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
  ;; Basiert auf sdl:with-events Code (SVN r1398)
  `(cond
     ,@(remove nil (mapcar #'(lambda (event)
			       (when (gethash (first event) sdl::*events*)
				 (sdl::expand-event sdl-event
						    (first event) ; Event-Type (z. B. :mouse-motion-event)
						    (gethash (first event) sdl::*events*)
						    (first (rest event)) ; Parameters
						    (rest (rest event))))) ; Handler-Body
			   events))))

(defvar *modelview-root-matrix* nil "Saved modelview matrix")
(defvar *texture-root-matrix* nil "Saved texture matrix")
(defvar *projection-root-matrix* nil "Saved projection matrix")

(defun set-perspective (w h)
  (with-matrix-mode :projection
    (glu:perspective 58.0 (/ w h) 0.1 1024.0)))

(defun init-gl (w h)
  (gl:clear-color 0 0 0 0)
  (gl:shade-model :smooth)
  ;; Perspektive
  (matrix-mode :projection
    (gl:load-identity)
    (set-perspective w h))
  ;; Mitte...
  (matrix-mode :modelview
    (gl:load-identity))
  ;; save matrices
  (setq *modelview-root-matrix* (gl:get-float :modelview-matrix)
	*texture-root-matrix* (gl:get-float :texture-matrix)
	*projection-root-matrix* (gl:get-float :projection-matrix)))

(defmacro with-standard-rendering (&body body)
  "Pushes the current matrices and GL attributes,
loads teh root matrices saved by #'init-gl and executes body with them
and finally restores the pushed matrices and GL attributes"
  `(progn
     (matrix-mode :texture
       (gl:push-matrix)
       (gl:load-matrix *texture-root-matrix*))
     (matrix-mode :projection
       (gl:push-matrix)
       (gl:load-matrix *projection-root-matrix*))
     (matrix-mode :modelview
       (gl:push-matrix)
       (gl:load-matrix *modelview-root-matrix*))
     (gl:push-attrib :all-attrib-bits)
     (gl:enable :cull-face)
     (gl:disable :clip-plane0 :clip-plane1 :clip-plane2 :clip-plane3)
     (unwind-protect (progn ,@body)
       (gl:pop-attrib)
       (matrix-mode :modelview (gl:pop-matrix))
       (matrix-mode :texture (gl:pop-matrix))
       (matrix-mode :projection (gl:pop-matrix)))))

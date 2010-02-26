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
  (setq *modelview-root-matrix* (gl:get-float :modelview-matrix)
	*texture-root-matrix* (gl:get-float :texture-matrix)
	*projection-root-matrix* (gl:get-float :projection-matrix)))     

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
       (with-matrix-mode :modelview
	 (gl:load-identity))		; sollte man das wirklich hier lassen?
       ;; restore our attributes
       (gl:push-attrib :all-attrib-bits)
       (gl:disable :clip-plane0 :clip-plane1 :clip-plane2 :clip-plane3) ; agar uses up to clip-plane3
       (gl:enable :cull-face)

       ;; this saves from Agar's distortion, but don't ask me why
       (with-matrix-mode :projection
	 (gl:with-pushed-matrix
	   (gl:load-identity)
	   (let ((viewport (gl:get-integer :viewport)))
	     (set-perspective (aref viewport 2) (aref viewport 3)))

	   ,@body

	   ))

       ;; restore Agar's attributes
       (gl:pop-attrib))))

#-agar
(defmacro non-agar-rendering (&body body)
  `(progn ,@body))

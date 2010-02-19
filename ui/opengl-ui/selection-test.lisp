(in-package skat-opengl-ui)

(defclass selection-test (module)
  ((seldraw :accessor seldraw :initform nil :documentation "Draw in selection mode")
   (mousepos :accessor mousepos :initform (list 0 0))
   (buffer :accessor buffer)
   (buffer-size :accessor buffer-size :initarg :buffer-size :initform 1024)))

(defmethod initialize-instance :after ((module selection-test) &key)
  "Allocates the selection buffer and makes it to be freed"
  (setf (buffer module) (cffi:foreign-alloc :uint :count (buffer-size module)))
  (let ((ptr (buffer module)))
    (trivial-garbage:finalize module
			      #'(lambda () (cffi:foreign-free ptr)))))

(let ((last-mm nil)
      (last-pm nil))
  (defun draw-this ()
    (gl:matrix-mode :modelview)
    (gl:disable :texture-2d)
    (gl:with-pushed-matrix
      (gl:load-identity)
      (gl:translate 0 0 -10)
					;    (break "modelview in draw: ~s" (gl:get-integer :modelview-matrix))
      (with-selname 1
	(gl:color 1 0 0)
	(gl:with-primitives :triangles
	  (gl:vertex 0 0)
	  (gl:vertex 1 0)
	  (gl:vertex 0.5 1.75)))
      (with-selname 2
	(gl:color 0 1 0)
	(gl:with-primitives :triangles
	  (gl:vertex 2 0)
	  (gl:vertex 3 0)
	  (gl:vertex 2.5 1.75)))
      (with-selname 3
	(with-selname 1
	  (gl:color 0 0 1)
	  (gl:with-primitives :triangles
	    (gl:vertex 2 3)
	    (gl:vertex 3 3)
	    (gl:vertex 2.5 4.75)))
	(with-selname 2
	  (gl:color 0 1 1)
	  (gl:with-primitives :triangles
	    (gl:vertex 2 3)
	    (gl:vertex 2.5 1.25)
	    (gl:vertex 3 3))))
      (let ((mm (gl:get-integer :modelview-matrix))
	    (pm (gl:get-integer :projection-matrix)))
	(unless (equalp last-mm mm)
	  (setf last-mm mm)
	  (format t "~%draw-this modelview: ~s" mm))
	(unless (equalp last-pm pm)
	  (setf last-pm pm)
	  (format t "~%draw-this projection: ~s" pm))))))

(let ((last-mm nil)
      (last-pm nil))
  (defmethod draw ((module selection-test))
    (if (seldraw module)
	(%dry-selection (first (mousepos module)) (second (mousepos module))
		   (buffer module) (buffer-size module) #'draw-this)
	(progn
	  (let ((mm (gl:get-integer :modelview-matrix))
		(pm (gl:get-integer :projection-matrix)))
	    (unless (equalp mm last-mm)
	      (setf last-mm mm)
	      (format t "~%draw modelview: ~s" mm))
	    (unless (equalp pm last-pm)
	      (setf last-pm pm)
	      (format t "~%draw projection: ~s" pm)))
	  ;; this saves from Agar's distortion, but don't ask me why
	  (with-matrix-mode :projection
	    (gl:with-pushed-matrix
	      (gl:load-identity)
	      (set-perspective 640 480)
	      ;; because actually the following format is never called,
	      ;; i. e. the projection matrix is the same as before
	      (let ((pm (gl:get-integer :projection-matrix)))
	      	(unless (equalp last-pm pm)
	      	  (format t "~%DRAW PROJECTION: ~s" pm)))
	      (draw-this)))))))
       
(defmethod handle-event ((module selection-test) event)
  ;; if case-event doesn't work with Lispbuilder, update Lispbuilder at least to svn-r1398
  (case-event event
    (:mouse-button-down-event (:x x :y y)
			      (setf (mousepos module) (list x y))
			      (setf (seldraw module) t)
			      (multiple-value-bind (result n)
			      	  (selection x y (buffer module) (buffer-size module) #'draw-this)
			      	(print result)
			      	(print n))
			      )
    (:mouse-motion-event (:x x :y y)
			 (when (seldraw module)
			   (setf (mousepos module) (list x y))))
    (:mouse-button-up-event ()
			    (setf (seldraw module) nil))))

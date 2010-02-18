(in-package skat-opengl-ui)

(defclass selection-test (module)
  ((buffer :accessor buffer)
   (buffer-size :accessor buffer-size :initarg :buffer-size :initform 1024)))

(defmethod initialize-instance :after ((module selection-test) &key)
  "Allocates the selection buffer and makes it to be freed"
  (setf (buffer module) (cffi:foreign-alloc :uint :count (buffer-size module)))
  (let ((ptr (buffer module)))
    (trivial-garbage:finalize module
			      #'(lambda () (cffi:foreign-free ptr)))))

(defmethod draw ((module selection-test))
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
	  (gl:vertex 3 3))))))

(defmethod handle-event ((module selection-test) event)
  (case-event event
    (:mouse-button-down-event (:x x :y y)
			      (multiple-value-bind (result n)
				  (selection x y (buffer module) (buffer-size module) #'draw module)
				(print result)
				(print n)))))

(in-package gl-ui)

(defclass send-button (module)
  ((texture :accessor texture)
   (click-handler :accessor click-handler-function)))

(defconstant +send-button+ 100)

(defmethod draw ((module send-button))
  (with-matrix-mode :modelview
    (gl:with-pushed-matrix
      (gl:load-identity)
      (gl:scale 2 1 1)
      (gl:translate 3 -4 -10)
      (when (slot-boundp module 'texture)
	(gl:enable :texture-2d)
	(gl:bind-texture :texture-2d (texture module)))
      (gl:color 1 1 1)
      (with-selname +send-button+
	(gl:with-primitives :quads
	  (gl:tex-coord 0 1) (gl:vertex 0 0)
	  (gl:tex-coord 1 1) (gl:vertex 0.4 0)
	  (gl:tex-coord 1 0) (gl:vertex 0.4 0.4)
	  (gl:tex-coord 0 0) (gl:vertex 0 0.4))))))

(defmethod handle-event ((module send-button) event)
  (case-event event
    (:mouse-button-down-event (:x x :y y)
			      (when (slot-boundp module 'click-handler)
				(let ((records (sort (select-gl-object x y #'draw module) #'< :key #'hit-record-max-z)))
				  (when (and records (= +send-button+ (car (hit-record-names-on-stack (car records)))))
				    (funcall (click-handler-function module))))))))

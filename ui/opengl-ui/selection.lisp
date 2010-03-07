(in-package gl-ui)

(defmacro with-selname (name &body body)
  `(progn
     (%gl:push-name ,name)
     (multiple-value-prog1 (progn ,@body)
       (%gl:pop-name))))

(cffi:define-foreign-type hit-records-type ()
  ((n-hits :accessor n-hits :initarg :n-hits))
  (:simple-parser hit-records)
  (:actual-type :pointer)
  (:documentation "A type to designate the hit record buffer of OpenGL Selection (5.2)"))

(defmethod print-object ((hit-records-type hit-records-type) stream)
  (print-unreadable-object (hit-records-type stream :type t)
    (princ "Hits=" stream)
    (princ (n-hits hit-records-type) stream)))

(defstruct hit-record n-names-on-stack min-z max-z names-on-stack)

(defmethod cffi:translate-from-foreign (buffer-ptr (type hit-records-type))
  "Parses the hit record buffer into a list of hit-record structures"
  (loop with ptr = buffer-ptr
     for n from 1 to (n-hits type)
     collect (let ((n-names (cffi:mem-aref ptr :uint))) ; during the last iteration the ptr was moved beyond the names array
	       (make-hit-record :n-names-on-stack n-names
				:min-z (cffi:mem-aref ptr :uint 1)
				:max-z (cffi:mem-aref ptr :uint 2)
				:names-on-stack (progn
						  ;; advance to names array
						  (cffi:incf-pointer ptr (* 3 (cffi:foreign-type-size :uint)))
						  ;; and collect the names
						  (loop for n from 1 to n-names
						     collect (cffi:mem-aref ptr :uint)
						     do (cffi:incf-pointer ptr (cffi:foreign-type-size :uint)))))))) ; advance to next item

(defun end-selection (new-mode buffer-ptr)
  "Switches glRenderMode to new-mode and returns the parsed selection buffer"
  (let ((n-hits (gl:render-mode new-mode)))
    (if (< n-hits 0)
	(warn "Selection buffer overflow!")
	(values (cffi:convert-from-foreign buffer-ptr `(hit-records :n-hits ,n-hits))
		n-hits))))

(defun selection (x y buffer buffer-size draw-function &rest fn-args)
  "Performs an OpenGL selection about 1px at Point (x, y) and uses
buffer with size buffer-size as the hit record buffer. draw-function should
perform the rendering from which objects can be selected when called with fn-args"
  (let ((viewport (gl:get-integer :viewport)))
    (%gl:select-buffer buffer-size buffer)
    (gl:render-mode :select)
    (%gl:init-names)
    (with-standard-rendering		; get rid of the Agar matrices
      (matrix-mode :projection
      	(gl:with-pushed-matrix		; push the projection matrix
      	  ;; get an equivalent projection matrix which only shows the Pixel about (x, y)
      	  (gl:load-identity)
      	  (glu:pick-matrix x (- (aref viewport 3) y) 1.0 1.0 viewport)
      	  (set-perspective (aref viewport 2) (aref viewport 3))
      	  (matrix-mode :modelview
      	     (apply draw-function fn-args))
	  (matrix-mode :projection))))
    (end-selection :render buffer)))

;; testing stuff

(let ((last-mm nil)
      (last-pm nil))
  (defun %dry-selection (x y buffer buffer-size draw-function &rest fn-args)
    (declare (ignore x y))
    (let ((viewport (gl:get-integer :viewport)))
      (%gl:select-buffer buffer-size buffer)
      ;; gl:render-mode left out
      (%gl:init-names)
      (with-standard-rendering		; get rid of the Agar matrices
	(matrix-mode :projection
	  (gl:with-pushed-matrix		; push the projection matrix
	    ;; get an equivalent projection matrix which only shows the Pixel about (x, y)
	    (gl:load-identity)
	    ;; glu:pick-matrix left out
	    (set-perspective (aref viewport 2) (aref viewport 3))
	    ;; ich konstatiere: die Matritzen sind hier noch korrekt
	    (let ((mm (gl:get-float :modelview-matrix))
		  (pm (gl:get-float :projection-matrix)))
	      (unless (equalp last-mm mm)
		(setf last-mm mm)
		(format t "~%dry selection modelview: ~s" mm))
	      (unless (equalp last-pm pm)
		(setf last-pm pm)
		(format t "~%dry selection projection: ~s" pm)))
	    (with-matrix-mode :modelview
	      (apply draw-function fn-args)))))
      (end-selection :render buffer))))

(defun select-gl-object (x y draw-function &rest fn-args)
  (cffi:with-foreign-object (buffer :uint 1024) ; Uint[1024]
    (apply #'selection x y buffer 1024 draw-function fn-args)))

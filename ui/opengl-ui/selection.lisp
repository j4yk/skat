(in-package gl-ui)

(defmacro with-selname (name &body body)
  `(progn
     (%gl:push-name ,name)
     (multiple-value-prog1 (progn ,@body)
       (%gl:pop-name))))

(cffi:define-foreign-type hit-records-type ()
  ((n-hits :accessor n-hits :initarg :n-hits))
  (:simple-parser hit-records)
  (:actual-type :pointer))

(defmethod print-object ((hit-records-type hit-records-type) stream)
  (print-unreadable-object (hit-records-type stream :type t)
    (princ "Hits=" stream)
    (princ (n-hits hit-records-type) stream)))

(defstruct hit-record n-names-on-stack min-z max-z names-on-stack)

(defmethod cffi:translate-from-foreign (buffer-ptr (type hit-records-type))
  (loop with ptr = buffer-ptr
     for n from 1 to (n-hits type)
     collect (let ((n-names (cffi:mem-aref ptr :uint))) ; during the last iteration the ptr was moved beyond the names array
	       (make-hit-record :n-names-on-stack n-names
				:min-z (cffi:mem-aref ptr :uint 1)
				:max-z (cffi:mem-aref ptr :uint 2)
				:names-on-stack (let ((names (cffi:incf-pointer ; advance to names array
							      ptr
							      (* 3 (cffi:foreign-type-size :uint)))))
						  (loop for n from 1 to n-names
						     collect (cffi:mem-aref names :uint)
						     do (cffi:incf-pointer ptr (cffi:foreign-type-size :uint)))))))) ; advance to next item

(defun end-selection (new-mode buffer-ptr)
  "Switches glRenderMode to new-mode and return the parsed selection buffer"
  ;; FIXME: momentan kommt immer nil zur�ck, n-hits immer = 0
  (let ((n-hits (gl:render-mode new-mode)))
    (if (< n-hits 0)
	(warn "Selection buffer overflow!")
	(values (cffi:convert-from-foreign buffer-ptr `(hit-records :n-hits ,n-hits))
		n-hits))))

(defun selection (x y buffer buffer-size draw-function &rest fn-args)
  (let ((viewport (gl:get-integer :viewport)))
    (%gl:select-buffer buffer-size buffer)
    (gl:render-mode :select)
    (%gl:init-names)
    (non-agar-rendering		; get rid of the Agar matrices
      (with-matrix-mode :projection
	(gl:with-pushed-matrix		; push the projection matrix
	  ;; get an equivalent projection matrix which only shows the Pixel about (x, y)
	  (gl:load-identity)
	  (glu:pick-matrix x (- (aref viewport 3) y) 1.0 1.0 viewport)
	  (set-perspective (aref viewport 2) (aref viewport 3))
	  ;; ich konstatiere: die Matritzen sind korrekt
	  (with-matrix-mode :modelview
	    (apply draw-function fn-args)))))
    (end-selection :render buffer)))


(in-package gl-ui)

(defclass test-module (module)
  ((textures-updated-p :accessor textures-updated-p :initform nil)
   (texture :accessor texture)
   (blue-tex :accessor blue-tex)))

(defmethod draw ((module test-module))
  (update-textures module)			; wenn es neues in dieser Funktion gibt, führe das aus
  (gl:enable :texture-2d)
  (with-matrix-mode :modelview
    (gl:load-identity)
    (gl:translate 0 0 -10)
    (gl:bind-texture :texture-2d (blue-tex module))
    (with-matrix-mode :texture
      (gl:load-identity))			; wahrscheinlich braucht man das gar nicht immer
;    (gl:matrix-mode :modelview)
    ;; Blaues Dreieck
    (gl:color 1 1 1)
    (gl:with-primitive :triangles
      (gl:tex-coord 0 1) (gl:vertex 0 2 -1)
      (gl:tex-coord 0 0) (gl:vertex 0 0 -1)
      (gl:tex-coord 1 0) (gl:vertex 1 0 -2))
    (gl:bind-texture :texture-2d (texture module))
    (with-matrix-mode :texture
      (gl:load-identity))			; siehe oben
;    (gl:matrix-mode :modelview)
    ;; verzerrte Testkarte
    (gl:color 1 1 1)
    (gl:with-primitive :quads
      (gl:tex-coord 0 1) (gl:vertex -3 -3)
      (gl:tex-coord 1 1) (gl:vertex 5 -3)
      (gl:tex-coord 1 0) (gl:vertex 5 0)
      (gl:tex-coord 0 0) (gl:vertex -3 0))
    ;; zeigen
    (gl:disable :texture-2d)))		; braucht man bestimmt auch nicht

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
	(setf blue-tex (create-solid-filled-texture 0 0 1))
	;; und eine aus dem Bild
	(setf texture (texture-from-bmp (merge-pathnames "diamonds7.bmp"))))
      (break "~a ~a" (texture module) (blue-tex module)))))

(defmethod free-textures ((mod test-module))
  (gl:delete-textures (loop for slot in (list 'texture 'blue-tex)
			 when (slot-boundp mod slot)
			 collect (prog1
				     (slot-value mod slot)
				   (slot-makunbound mod slot)))))

(defun simple-sdl-test ()
  (let ((ui (make-instance 'opengl-ui))
	(testm (make-instance 'test-module)))
    (push testm (modules ui))
    (sdl:with-init ()
      (skat-window)
      (setf (sdl:frame-rate) 2)
      (init-gl 640 480)
      (sdl:show-cursor :enable)		; mit Cursor bitte
      (update-textures testm)
      (unwind-protect
	   (standard-main-loop ui)
	(free-textures testm)))))

(in-package skat-opengl-ui)

(defclass cards-module (module)
  ((own-cards :accessor cards :type list :initform nil)
   (left-cards :accessor cards :type list :initform nil)
   (right-cards :accessor cards :type list :initform nil)
   (back-texture :accessor back-texture :type fixnum))
  (:documentation "Module zum Zeichnen der Karten und zum Verarbeiten kartenspezifischer Aktionen"))

(defun draw-card-here (cards-module texture back-p)
  (when (and back-p (slot-boundp cards-module 'back-texture)) (gl:bind-texture :texture-2d (back-texture cards-module)))
  (when texture (gl:bind-texture :texture-2d texture))
  (gl:color 1 1 1)
  (gl:with-primitives :polygon
    (gl:tex-coord 0 1) (gl:vertex -3 -3) ; unten links
    (gl:tex-coord 1 1) (gl:vertex  5 -3) ; unten rechts
    (gl:tex-coord 1 0) (gl:vertex  5  0) ; oben rechts
    (gl:tex-coord 0 0) (gl:vertex -3  0) ; oben links
    ))

(defmethod draw ((module cards-module))
  "Zeichnet die Karten"
  (non-agar-rendering
    (gl:matrix-mode :modelview)
    (gl:disable :texture-2d)
    (gl:load-identity)
    (gl:translate 0 0 -5)
    (gl:color 1 1 1)
    (gl:with-primitives :triangles
      (gl:vertex 0 0 0)
      (gl:vertex 1 1 0)
      (gl:vertex -2 2 0))))


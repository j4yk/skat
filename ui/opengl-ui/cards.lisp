(in-package skat-opengl-ui)

(defclass cards (module)
  ((own-cards :accessor cards :type list :initform nil)
   (left-cards :accessor cards :type list :initform nil)
   (right-cards :accessor cards :type list :initform nil)
   (back-texture :accessor back-texture :type fixnum)
   (textures :accessor textures :type list :initform nil))
  (:documentation "Module zum Zeichnen der Karten und zum Verarbeiten kartenspezifischer Aktionen"))

(defun add-texture (module name texture)
  (setf (getf (textures module) name)
	texture)
  ;; automatically delete texture on module garbage collect
  (trivial-garbage:finalize
   module
   #'(lambda ()
       (gl:delete-textures (list texture)))))

(defmethod load-textures ((module cards))
  "Lädt die Texturen"
  (map nil
       #'(lambda (name texture)
	   (add-texture module name texture))
       (list :diamonds7 :diamonds8 :diamonds9 :diamondsQ
	     :diamondsK :diamonds10 :diamondsA :diamondsJ)
       (loop for val from 0.8 downto 0.1 by 0.1
	  collect (create-solid-filled-texture 1 val val))))

(defmethod initialize-instance :after ((module cards) &key)
  (load-textures module))

(defstruct card texture selection-name)

(defun own-cards-selname (nthcard)
  (let ((own-cards-offset 1000))
    (+ own-cards-offset nthcard)))

(defun draw-card-here (cards texture back-p selection-name)
  (when (and back-p (slot-boundp cards 'back-texture))
    (gl:bind-texture :texture-2d (back-texture cards)))
  (when texture
    (gl:bind-texture :texture-2d texture))
  (gl:color 1 1 1)			; Textur unverändert
  (gl:with-pushed-matrix
    (let ((f (/ 1 3)))
      (gl:scale f f f))
    (with-pushed-selname selection-name
      ;; Fläche
      (gl:with-primitives :polygon
	(gl:tex-coord 0 1) (gl:vertex (/ -6 2) (/ -9 2)) ; unten links
	(gl:tex-coord 1 1) (gl:vertex (/  6 2) (/ -9 2)) ; unten rechts
	(gl:tex-coord 1 0) (gl:vertex (/  6 2) (/  9 2)) ; oben rechts
	(gl:tex-coord 0 0) (gl:vertex (/ -6 2) (/  9 2))) ; oben links
      ;; Rahmen
      (gl:color 0 0 0)
      (gl:with-primitives :line-loop
	(gl:vertex (/ -6 2) (/ -9 2)) ; unten links
	(gl:vertex (/  6 2) (/ -9 2)) ; unten rechts
	(gl:vertex (/  6 2) (/  9 2)) ; oben rechts
	(gl:vertex (/ -6 2) (/  9 2)))))) ; oben links

(defun draw-hand (module cards)
  "Zeichnet eine aufgefaltete Hand von Karten"
  (gl:with-pushed-matrix
   (gl:translate 0 -5 0)
    (let ((ncards (length cards))
	  (dzrot 7)
;	  (dyrot 5)
	  (backstep -0.1))
      (loop
	 for n from 0 to (1- ncards)
	 and card in cards
	 do (gl:with-pushed-matrix
;	      (gl:rotate (* (+ (- (/ ncards 2)) n) dyrot) 0 1 0)
	      (gl:rotate (* (+ (- (/ ncards 2)) n) dzrot) 0 0 1)
	      (gl:translate 0 5 (* n backstep))
	      (draw-card-here module
			      (getf (textures module)
					   (card-texture card))
			      nil
			      (own-card-selname n)))))))

(defmethod draw ((module cards))
  "Zeichnet die Karten"
  (gl:matrix-mode :modelview)
  (gl:enable :texture-2d)
  (gl:load-identity)
  (gl:translate 0 0 -10)
  (gl:color 1 1 1)
  (gl:with-pushed-matrix
					;     (gl:rotate -20 1 0 0)		; 45° nach vorn geneigt
    (gl:translate 0 -2 0)
    (draw-hand module (list (make-card :texture :diamonds7)
			    (make-card :texture :diamonds8)
			    (make-card :texture :diamonds9)
			    (make-card :texture :diamondsQ)
			    (make-card :texture :diamondsK)
			    (make-card :texture :diamonds10)
			    (make-card :texture :diamondsA)
			    (make-card :texture :diamondsJ)))))

(defmethod handle-event ((module cards) event)
  (case-event event
    (:mouse-button-down-event (:x x :y y)
			      (declare (ignore x y))
			      ;; selection --> welche Karte?
			      )
    (:mouse-button-up-event (:x x :y y)
			    (declare (ignore x y))
			    ;; selection --> welche Karte?
			    )))
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
       (list :d7 :d8 :d9 :dq :dk :d10 :da :dj
	     :h7 :h8 :h9 :hq :hk :h10 :ha :hj
	     :s7 :s8 :s9 :sq :sk :s10 :sa :sj
	     :c7 :c8 :c9 :cq :ck :c10 :ca :cj)
       (let ((*default-pathname-defaults* (merge-pathnames "resources/cards/")))
	 (loop for suit in (list "diamond" "heart" "spade" "club")
	    append (loop for rank in (list "7" "8" "9" "10" "queen" "king" "1" "jack")
		      collect (sdl-surface-to-gl-texture
			       (sdl-image:load-image
				(merge-pathnames
				 (concatenate 'string rank "_" suit ".png")))))))))

(defmethod initialize-instance :after ((module cards) &key)
  (load-textures module))

(defstruct card texture selection-name)

(defun own-card-selname (nthcard)
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
    (with-selname selection-name
      ;; Fläche
      (gl:with-primitives :polygon
	(gl:tex-coord 0 1) (gl:vertex (/ -6 2) (/ -9 2)) ; unten links
	(gl:tex-coord 1 1) (gl:vertex (/  6 2) (/ -9 2)) ; unten rechts
	(gl:tex-coord 1 0) (gl:vertex (/  6 2) (/  9 2)) ; oben rechts
	(gl:tex-coord 0 0) (gl:vertex (/ -6 2) (/  9 2)))))) ; oben links


(defun draw-hand (module cards)
  "Zeichnet eine aufgefaltete Hand von Karten"
  (gl:with-pushed-matrix
   (gl:translate 0 -5 0)
    (let ((ncards (length cards))
	  (dzrot 7)
	  (dz 0.1))
      (loop
	 for n from 1 to ncards
	 and card in cards
	 do (gl:with-pushed-matrix
	      (gl:rotate (* (- (+ (/ ncards 2)) n) dzrot) 0 0 1)
	      (gl:translate 0 5 (* n dz))
	      (draw-card-here module
			      (getf (textures module)
					   (card-texture card))
			      nil
			      (own-card-selname n)))))))

(defmethod draw ((module cards))
  "Zeichnet die Karten"
  (gl:matrix-mode :modelview)
  (gl:enable :texture-2d)
;  (gl:enable :blend)
;  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:enable :alpha-test)
  (gl:alpha-func :greater 0.1)
  (gl:load-identity)
  (gl:translate 0 0 -10)
  (gl:color 1 1 1)
  (gl:with-pushed-matrix
    (gl:translate 0 -2 0)
    (draw-hand module (list (make-card :texture :d7)
			    (make-card :texture :d8)
			    (make-card :texture :d9)
			    (make-card :texture :dq)
			    (make-card :texture :dk)
			    (make-card :texture :d10)
			    (make-card :texture :da)
			    (make-card :texture :dj))))
;  (gl:disable :blend)
  (gl:disable :alpha-test)
  (gl:disable :texture-2d))

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
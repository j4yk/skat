(in-package skat-opengl-ui)

(defclass cards (module)
  ((own-cards :accessor cards :type list :initform nil)
   (left-cards :type list :initform nil)
   (right-cards :type list :initform nil)
   (textures :accessor textures :type list :initform nil :documentation "Property list of texture names and IDs")
   (select :accessor select-p :initform nil :documentation "Controls whether selection is performed on clicks or not")
   (n-max-select :accessor n-max-select :initform 1 :documentation "How many cards can be selected simultaneously")
   (selected-cards :reader selected-cards :initform nil :documentation "The currently selected cards"))
  (:documentation "Module zum Zeichnen der Karten und zum Verarbeiten kartenspezifischer Aktionen"))

(defmethod remove-cards ((module cards) cards)
  "Recursively removes the supplied cards from the player's hand"
  (if (null cards)
      (values)
      (progn
	(setf (cards module) (delete (first cards) (cards module) :test #'equalp))
	(remove-cards module (rest cards)))))

(defmethod toggle-selected-card ((module cards) card)
  "If the card is not yet selected it will be added to the list of selected cards.
If the card is already selected it will be removed from that list."
  (when (member nil (selected-cards module))
    (warn "Removing NIL from (selected-cards cards-module)")
    (setf (slot-value module 'selected-cards) (delete nil (selected-cards module))))
  (let ((cards (selected-cards module)))
    (if (member card cards :test #'equalp)
	(setf (slot-value module 'selected-cards) (delete card cards :test #'equalp))
	(push card (slot-value module 'selected-cards)))))

(defmethod clear-selected-cards ((module cards))
  "Deselects all cards"
  (setf (slot-value module 'selected-cards) nil))

(defconstant +own-cards+ 1000 "Selection name for the player's own cards")

(defun own-card-selname (nthcard)
  "Returns the specific selection name for the card at this position in the hand"
  (+ +own-cards+ nthcard))

(defun add-texture (module name texture)
  "Adds a texture to (textures module). The texture will be freed when module is finalized."
  (setf (getf (textures module) name)
	texture)
  ;; automatically delete texture on module garbage collect
  (trivial-garbage:finalize
   module
   #'(lambda ()
       (gl:delete-textures (list texture)))))

(defmethod load-textures ((module cards))
  "Lädt die Texturen"
  (let ((*default-pathname-defaults* (merge-pathnames "resources/cards/")))
    (map nil
	 #'(lambda (name texture)
	     (add-texture module name texture))
	 (list :d7 :d8 :d9 :dq :dk :d10 :da :dj
	       :h7 :h8 :h9 :hq :hk :h10 :ha :hj
	       :s7 :s8 :s9 :sq :sk :s10 :sa :sj
	       :c7 :c8 :c9 :cq :ck :c10 :ca :cj)
	 (loop for suit in (list "diamond" "heart" "spade" "club")
	    append (loop for rank in (list "7" "8" "9" "queen" "king" "10" "1" "jack")
		      collect (sdl-surface-to-gl-texture
			       (sdl-image:load-image
				(merge-pathnames
				 (concatenate 'string rank "_" suit ".png")))))))
    ;; backside of the cards
    (add-texture module :backside (sdl-surface-to-gl-texture (sdl-image:load-image (merge-pathnames "back.png"))))))

(defmethod initialize-instance :after ((module cards) &key)
  "Loads the card textures"
  (load-textures module))

(defstruct card (card #!D7 :type kern:card) selection-name)

(defun card-to-texture-name (card)
  "Returns the texture name for this specific card"
  (check-type card kern:card)
  (intern (subseq (with-output-to-string (s) (kern:print-card card s)) 2) 'keyword))

(defun draw-card-here (module texture selection-name &key back-p selected-p)
  (when (and back-p (getf (textures module) :backside))
    (gl:bind-texture :texture-2d (getf (textures module) :backside)))
  (when texture
    (gl:bind-texture :texture-2d texture))
  (when selected-p
    (gl:enable :color-logic-op)
    (gl:logic-op :copy-inverted))
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
	(gl:tex-coord 0 0) (gl:vertex (/ -6 2) (/  9 2))))) ; oben links
  (when selected-p
    (gl:disable :color-logic-op)))


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
					   (card-to-texture-name card))
			      (own-card-selname n)
			      :selected-p (member card (selected-cards module) :test #'equalp)))))))

(defmethod draw ((module cards))
  "Zeichnet die Karten"
  (declare (optimize debug))
  (gl:enable :texture-2d)
  (gl:enable :alpha-test)
  (gl:tex-env :texture-env :texture-env-mode :modulate)
  (gl:alpha-func :greater 0.1)
;;  (gl:enable :blend)
;;  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (matrix-mode :modelview
    (gl:load-identity)
    (gl:translate 0 0 -10)
    (gl:color 1 1 1)
    (gl:with-pushed-matrix
      (gl:translate 0 -2 0)
      (with-selname 1000
	(draw-hand module (cards module)))))
;;  (gl:disable :blend)
  (gl:disable :alpha-test)
  (gl:disable :texture-2d))

(defmethod select-card ((module cards) x y)
  "Does a selection at P(x,y) and returns the card that the clicked object represents"
  (declare (optimize (debug 3)))
  (let ((hit-records (sort (select-gl-object x y #'draw module) #'< :key #'hit-record-max-z)))
    (declare (optimize debug))
    (when hit-records
      (let ((record (dolist (r hit-records) ; look for a card hit
		      (declare (optimize debug))
		      (when (= +own-cards+ (car (hit-record-names-on-stack r))) ; card hit
			(return r)))))
	(declare (optimize debug))
	(when record
	  (let ((nth-card (1- (- (second (hit-record-names-on-stack record)) +own-cards+)))) ; offset
	    (nth nth-card (cards module))))))))

(defmethod handle-event ((module cards) event)
  (case-event event
    (:mouse-button-down-event (:x x :y y)
			      (when (select-p module)
				(let ((card (select-card module x y)))
				  (when card
				    (unless (and (= (length (selected-cards module)) (n-max-select module))
						 (not (member card (selected-cards module) :test #'equalp)))
				      (toggle-selected-card module card))))))
    (:mouse-button-up-event (:x x :y y)
			    (declare (ignore x y))
			    ;; selection --> welche Karte?
			    )))

;; convenience functions

(defmethod prepare-choose-skat ((module cards))
  "Prepare the cards module to let the player choose two cards for the skat"
  (setf (select-p module) t		; make cards selectable
	(n-max-select module) 2))

(defmethod end-choose-skat ((module cards))
  "Make cards no longer selectable and clear selection"
  (clear-selected-cards module)
  (setf (select-p module) nil))

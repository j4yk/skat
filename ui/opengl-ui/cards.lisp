(in-package skat-opengl-ui)

(defclass cards (module)
  ((own-cards :accessor cards :type list :initform nil)
   (left-cards :type list :initform nil)
   (right-cards :type list :initform nil)
   (middle-stack :type list :initform nil)
   (game :accessor game :initform :grand)
   (textures :accessor textures :type list :initform nil :documentation "Property list of texture names and IDs")
   (card-display-list) (card-reversed-display-list)
   (select :accessor select-p :initform nil :documentation "Controls whether selection is performed on clicks or not")
   (n-max-select :accessor n-max-select :initform 1 :documentation "How many cards can be selected simultaneously")
   (selected-cards :reader selected-cards :initform nil :documentation "The currently selected cards")
   (choose-card-p :accessor choose-card-p :initform nil :documentation "Indicates whether a card is to be chosen")
   (candidate-card :initform nil :documentation "The card on which a click was started")
   (last-mouse-pos :accessor last-mouse-pos :initform #(0 0) :documentation "Saves the last known mouse position"))
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
  (load-textures module)
  ;; create display lists for the two types of cards (front and back)
  (let ((offset (gl:gen-lists 2)))
    (with-slots (card-display-list card-reversed-display-list) module
      (setf card-display-list (+ offset 0)
	    card-reversed-display-list (+ offset 1))
      (gl:with-new-list (card-display-list :compile)
	(gl:with-primitives :quads
	  ;; cards drawn from the front side are drawn counterclockwise
	  (gl:tex-coord 0 1) (gl:vertex (/ -6 2) (/ -9 2)) ; unten links
	  (gl:tex-coord 1 1) (gl:vertex (/  6 2) (/ -9 2)) ; unten rechts
	  (gl:tex-coord 1 0) (gl:vertex (/  6 2) (/  9 2)) ; oben rechts
	  (gl:tex-coord 0 0) (gl:vertex (/ -6 2) (/  9 2)) ; oben links
	  ))
      (gl:with-new-list (card-reversed-display-list :compile)	
	(gl:with-primitives :quads
	  ;; draw backside cards clockwise
	  ;; because else the texture directs away from the viewer
	  ;; and the card would be invisible
	  (gl:tex-coord 0 1) (gl:vertex (/ -6 2) (/ -9 2)) ; unten links
	  (gl:tex-coord 0 0) (gl:vertex (/ -6 2) (/  9 2)) ; oben links
	  (gl:tex-coord 1 0) (gl:vertex (/  6 2) (/  9 2)) ; oben rechts
	  (gl:tex-coord 1 1) (gl:vertex (/  6 2) (/ -9 2)) ; unten rechts
	  )))))

(defstruct ui-card
  "A card object that is to be drawn"
  (card #!D7 :type kern:card)
  selection-name
  from
  covered-p)

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
      (with-slots (card-display-list card-reversed-display-list) module
	(if back-p
	    (gl:call-list card-reversed-display-list)
	    (gl:call-list card-display-list)))))
  (when selected-p
    (gl:disable :color-logic-op)))

(defun candidate-card-p (module card)
  (equalp card (slot-value module 'candidate-card)))

(defun card-at-last-mouse-pos (module)
  (with-slots (last-mouse-pos) module
    (select-card module (aref last-mouse-pos 0) (aref last-mouse-pos 1))))

(defun card-selected-p (module card)
  (member card (selected-cards module) :test #'equalp))

(defun draw-hand (module cards selname-generator-fn)
  "Zeichnet eine aufgefaltete Hand von Karten"
  (with-pushed-matrix :modelview
   (gl:translate 0 -5 0)
    (let ((ncards (length cards))
	  (dzrot 7)
	  (dz 0.1))
      (loop
	 for n from 1 to ncards
	 and card in cards
	 do (with-pushed-matrix :modelview
	      (gl:rotate (* (- (+ (/ ncards 2)) n) dzrot) 0 0 1)
	      (gl:translate 0 5 (* n dz))
	      (draw-card-here module
			      (getf (textures module)
				    (card-to-texture-name (ui-card-card card)))
			      (funcall selname-generator-fn n)
			      :back-p (ui-card-covered-p card)
			      :selected-p (or (card-selected-p module card) nil)))))))

;; convenience functions

(defmethod select-skat ((module cards))
  "Prepare the cards module to let the player choose two cards for the skat"
  (setf (select-p module) t		; make cards selectable
	(n-max-select module) 2))

(defmethod end-choose-skat ((module cards))
  "Make cards no longer selectable and clear selection"
  (clear-selected-cards module)
  (setf (select-p module) nil))

(defmethod add-cards ((module cards) cards)
  (setf (cards module)
	(mapcar #'(lambda (card)
		    (make-ui-card :card card :covered-p nil))
		(kern:sort-cards (nconc (mapcar #'ui-card-card (cards module)) cards) (game module)))))

(defmethod choose-card ((module cards))
  "Enables the handling of clicks on the cards"
  (setf (choose-card-p module) t))

(defmethod select-card ((module cards) x y)
  "Does a selection at P(x,y) and returns the card that the clicked object represents"
  (declare (optimize debug))
  (let ((hit-records (select (ui module) x y)))
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

(defmethod send-card ((module cards) card)
  "Sends the card to the kernel to play it, so also remove it from the hand and
prohibit further reaction on clicks on the cards"
  (play-card (ui module) card)
  (setf (choose-card-p module) nil)
  (remove-cards module (list card)))

(defmethod middle-stack-push ((module cards) card)
  "Pushes another card onto the the stack in the middle of the table"
  (error "not implemented"))

(defmethod card-played ((module cards) card)
  "Pushes the card onto the middle stack."
  (middle-stack-push module card))

(defun draw-table ()
  (with-pushed-matrix
    :modelview
    (gl:translate 0 -3 -15)
    (gl:with-pushed-attrib (:texture-2d :depth-test)
      (gl:disable :texture-2d)
      (gl:disable :depth-test)
      (gl:color 0 0.6 0)
      (gl:with-primitives :triangle-fan
	(gl:vertex 0 0 0)
	(let ((r 15))
	  (loop for alpha from (* 2 pi) downto 0 by (/ pi 36)
	     do (let ((x (* r (cos alpha)))
		      (z (* r (sin alpha))))
		  (gl:vertex (- x) 0 (- z)))))))
    (gl:color 1 1 1)
    (gl:with-primitives :points
      (gl:vertex 0 0 0))))

;; Module methods

(defmethod draw ((module cards))
  "Zeichnet die Karten"
  (declare (optimize debug))
  (draw-table)
  (gl:enable :texture-2d)
  (gl:enable :alpha-test)
  (gl:tex-env :texture-env :texture-env-mode :modulate)
  (gl:alpha-func :greater 0.1)
  (gl:color 1 1 1)
  (with-pushed-matrix
    :modelview
    (gl:translate 0 0 -8)
    (gl:translate 0 -3 0)
    (gl:rotate -20 1 0 0)
    (gl:rotate 10 0 1 0)
    (with-selname 1000
      (draw-hand module (cards module) #'own-card-selname)))
  (with-pushed-matrix
    :modelview
    (gl:translate 3 3 -15)
    (with-pushed-matrix
      (gl:translate -5 0 0)
      (draw-card-here module (getf (textures module) (card-to-texture-name #!d7)) 99998 :back-p nil))
    (gl:rotate 180 0 1 0) 		; volle Wende
    (draw-card-here module nil 99999 :back-p t))
  (gl:disable :alpha-test)
  (gl:disable :texture-2d))

(defmethod handle-event ((module cards) event)
  (case-event event
    (:mouse-button-down-event (:x x :y y)
			      (cond ((select-p module)
				     ;; select a card if the maximum number of selectable cards
				     ;; has not yet been reached, if the card is already selected
				     ;; unselect it
				     (let ((card (select-card module x y)))
				       (when card
					 (unless (and (= (length (selected-cards module)) (n-max-select module))
						      (not (member card (selected-cards module) :test #'equalp)))
					   (toggle-selected-card module card)))))
				    ((choose-card-p module)
				     ;; remember a card that was clicked on
				     (let ((card (select-card module x y)))
				       (when card
					 (with-slots (candidate-card) module
					   (setf candidate-card card)))))))
    (:mouse-motion-event (:x x :y y)
			 ;; save mouse pos
			 (with-slots (last-mouse-pos) module
			   (setf (aref last-mouse-pos 0) x
				 (aref last-mouse-pos 1) y)))
    (:mouse-button-up-event (:x x :y y)
			    (cond ((choose-card-p module)
				   ;; if a clicked card is still the same
				   ;; as the one where the click began then send it
				   ;; else discard that card
				   (let ((card (select-card module x y)))
				     (if card
					 (with-slots (candidate-card) module
					   (if (equalp card candidate-card)
					       (send-card module card)
					       (slot-makunbound module 'candidate-card)))
					 (slot-makunbound module 'candidate-card))))))))
  
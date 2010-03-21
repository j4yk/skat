(in-package skat-opengl-ui)

(defclass cards (module)
  ((own-cards :accessor cards :type list :initform nil)
   (own-tricks :initform nil)
   (left-cards :type list :initform nil)
   (left-tricks :initform nil)
   (right-cards :type list :initform nil)
   (right-tricks :initform nil)
   (middle-stack :type list :initform nil)
   (last-trick :accessor last-trick :type list :initform nil)
   (game :accessor game :initform :grand)
   (textures :accessor textures :type list :initform nil :documentation "Property list of texture names and IDs")
   (card-display-list) (card-reversed-display-list)
   (show-last-trick-p :accessor show-last-trick-p :initform nil)
   (select :accessor select-p :initform nil :documentation "Controls whether selection is performed on clicks or not")
   (n-max-select :accessor n-max-select :initform 1 :documentation "How many cards can be selected simultaneously")
   (selected-cards :reader selected-cards :initform nil :documentation "The currently selected cards")
   (choose-card-p :accessor choose-card-p :initform nil :documentation "Indicates whether a card is to be chosen")
   (candidate-card :initform nil :documentation "The card on which a click was started")
   (queued-actions :accessor queued-actions :initform nil
		   :documentation "Function calls to be done if some
		   timeouts have passed.")
   (timeouts :accessor timeouts)
   (last-mouse-pos :accessor last-mouse-pos :initform #(0 0) :documentation "Saves the last known mouse position"))
  (:documentation "Module zum Zeichnen der Karten und zum Verarbeiten kartenspezifischer Aktionen"))

(defmacro queued-args (module timeout-ident)
  "Shorthand for the cdr of the timeout assoc in (queued-actions module)"
  `(cdr (assoc ,timeout-ident (queued-actions ,module))))

(defun timeout-scheduled-p (module timeout-ident)
  "Returns t if the timeout referenced with timeout-ident is currenly due."
  (ag:with-locked-timeouts (null-pointer)
    (ag:timeout-is-scheduled (null-pointer) (getf (timeouts module) timeout-ident))))

(defmethod schedule-timeout ((module cards) timeout-ident ival &rest args)
  "Pushes the args onto the stack of queued-actions in the timeout-ident assoc
and schedules the timeout with Agar"
  (setf (queued-args module timeout-ident)
	(nconc (queued-args module timeout-ident) (list (cons (+ (ag:get-ticks) ival) args))))
  (let ((timeout (getf (timeouts module) timeout-ident)))
    (unless (timeout-scheduled-p module timeout-ident)
      (ag:schedule-timeout (null-pointer) timeout ival))))

(eval-when (:compile-toplevel :load-toplevel)
  (defvar *delayed-methods* nil "The names of methods that use the delaying mechanism"))

(defun delay-methods (name lambda-list body)
  (let ((do-name (conc-symbols 'do- name))
	(delay-name (conc-symbols 'delay- name))
	(timeout-ident (to-keyword name))
	(module-param (caar lambda-list))
	(ival-arg (gensym "IVAL"))
	(args (get-args-from-specialized-lambda-list lambda-list)))
    (pushnew name *delayed-methods*)
    (if (find-if (rcurry #'member '(&optional &key &rest)) args)
	(error "Define-delayed cannot handle optional, key and rest arguments")
	(values
	 `(progn
	    (defmethod ,delay-name ,(append (list ival-arg) lambda-list)
	      ,(format nil "Schedules a call to ~a" do-name)
	      (schedule-timeout ,module-param ,timeout-ident ,ival-arg ,@args))
	    (defmethod ,do-name ,lambda-list ,@body))
	 args do-name delay-name timeout-ident))))

(defmacro define-delayable (name lambda-list &body body)
  (multiple-value-bind (forms args do-name delay-name) (delay-methods name lambda-list body)
    (declare (ignore delay-name))
    (append forms (list `(defmethod ,name ,lambda-list
			   ,(if (stringp (car body))
				(car body)
				(if (stringp (cadr body))
				    (cadr body)
				    nil))
			   (,do-name ,@args))))))

(defmacro define-delayed (name lambda-list delay &body body)
  "Defines a method that should only be executed after a specific delay"
  (multiple-value-bind (forms args do-name delay-name) (delay-methods name lambda-list body)
    (append forms (list `(defmethod ,name ,lambda-list
			   ,(format nil "Schedules a call to ~a in ~a ticks" do-name delay)
			   (,delay-name ,delay ,@args))))))

(let ((counter 1))
  (define-delayed delayed-test ((module cards)) 2000
    "A test delay function, shows up a window when body is executed"
    (let ((win (ag:window-new)))
      (ag:window-set-caption win "~a" counter)
      (ag:set-window-position win :tl nil)
      (ag:new-label win nil (format nil "~a" counter))
      (incf counter)
      (ag:window-show win))))

(defmethod remove-cards ((module cards) ui-cards)
  "Deletes cards from the player's hand."
  (dolist (card ui-cards)
    (setf (cards module) (delete card (cards module) :test #'equalp))))

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
	texture))

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

(defun candidate-card-p (module card)
  (equalp card (slot-value module 'candidate-card)))

(defun card-at-last-mouse-pos (module)
  (with-slots (last-mouse-pos) module
    (select-card module (aref last-mouse-pos 0) (aref last-mouse-pos 1))))

(defun card-selected-p (module card)
  (member card (selected-cards module) :test #'equalp))

;; graphics

(defvar card-height 3.0)
(defvar card-width 2.0)

(declaim (single-float card-height card-width))

(defmethod create-display-lists ((module cards))
  ;; create display lists for the two types of cards (front and back)
  (let ((offset (gl:gen-lists 2)))
    (with-slots (card-display-list card-reversed-display-list) module
      (setf card-display-list (+ offset 0)
	    card-reversed-display-list (+ offset 1))
      (gl:with-new-list (card-display-list :compile)
	(gl:with-primitives :quads
	  ;; cards drawn from the front side are drawn counterclockwise
	  (gl:tex-coord 0 1) (gl:vertex (/ (- card-width) 2) (/ (- card-height) 2)) ; unten links
	  (gl:tex-coord 1 1) (gl:vertex (/ card-width 2) (/ (- card-height) 2)) ; unten rechts
	  (gl:tex-coord 1 0) (gl:vertex (/ card-width 2) (/ card-height 2)) ; oben rechts
	  (gl:tex-coord 0 0) (gl:vertex (/ (- card-width) 2) (/ card-height 2)) ; oben links
	  ))
      (gl:with-new-list (card-reversed-display-list :compile)	
	(gl:with-primitives :quads
	  ;; draw backside cards clockwise
	  ;; because else the texture directs away from the viewer
	  ;; and the card would be invisible
	  (gl:tex-coord 0 1) (gl:vertex (/ (- card-width) 2) (/ (- card-height) 2)) ; unten links
	  (gl:tex-coord 0 0) (gl:vertex (/ (- card-width) 2) (/ card-height 2)) ; oben links
	  (gl:tex-coord 1 0) (gl:vertex (/ card-width 2) (/ card-height 2)) ; oben rechts
	  (gl:tex-coord 1 1) (gl:vertex (/ card-width 2) (/ (- card-height) 2)) ; unten rechts
	  )))))

(defun draw-card-here (module texture selection-name &key back-p selected-p)
  (declare (optimize speed)
	   (cards module) (integer texture selection-name))
  (gl:bind-texture :texture-2d
		   (cond ((and back-p (getf (textures module) :backside))
			  (getf (textures module) :backside))
			 (texture texture)))
  (when selected-p
    (gl:enable :color-logic-op)
    (gl:logic-op :copy-inverted))
  (gl:color 1 1 1)			; Textur unverändert
  (gl:with-pushed-matrix
    (with-selname selection-name
      ;; Fläche
      (with-slots (card-display-list card-reversed-display-list) module
	(if back-p
	    (gl:call-list card-reversed-display-list)
	    (gl:call-list card-display-list)))))
  (when selected-p
    (gl:disable :color-logic-op)))

(defmethod draw-ui-card-here ((module cards) ui-card selname selected-p)
  (declare (optimize speed)
	   (ui-card ui-card) (integer selname))
  (draw-card-here module
		  (getf (textures module)
			(card-to-texture-name (ui-card-card ui-card)))
		  (or selname 9000)
		  :back-p (ui-card-covered-p ui-card)
		  :selected-p selected-p))

(defun draw-hand (module cards selname-generator-fn)
  "Zeichnet eine aufgefaltete Hand von Karten"
  (declare (optimize speed)
	   (cards module) (list cards)
	   ((function (integer) integer) selname-generator-fn))
  (with-pushed-matrix :modelview
    (let ((ncards (length cards))
	  (dzrot (float (/ 90 8)))		; 8 cards make up 90°
	  (dz 0.1))
      (loop
	 for n from 1 to ncards
	 and card in cards
	 do (with-pushed-matrix
	      :modelview
	      (let* (;; create a fan with the hand:
		     (rot-radius (* 0.7 (float card-height))) ; turn a little below the lower edge of the cards
		     (zrot-angle (* (- (+ (/ (float ncards) 2)) (float n)) dzrot))
		     (zrot-angle-rad (- (/ (* zrot-angle pi) 180)))
		     ;; bending with the hand:
		     (bend-radius (* 1.2 card-height))
		     ;; translations
		     (delta-x (* rot-radius (sin (the (float 0.0 4.0) zrot-angle-rad))))
		     (delta-y (- (* rot-radius (- 1 (cos (the (float 0.0 4.0) zrot-angle-rad))))))
		     (delta-z (+ (* (float n) dz) (- bend-radius (sqrt (- (* bend-radius bend-radius) (* delta-x delta-x))))))
		     ;; rotation angle for bending
		     (bend-rot-angle (- (* (/ (acos (/ delta-x bend-radius)) pi) 180) 90)))
		(gl:translate delta-x delta-y delta-z)
		(gl:rotate bend-rot-angle 0 1 0) ; bend rotate in-place
		(gl:rotate zrot-angle 0 0 1)) ; fan rotate in-place
	      (draw-ui-card-here module card (funcall selname-generator-fn n)
				 (card-selected-p module card)))))))

(defun draw-table ()
  (declare (optimize speed))
  (gl:disable :texture-2d)
  (gl:disable :depth-test)
  (gl:color 0 0.6 0)
    (gl:with-primitives :triangle-fan
      (gl:vertex 0 0 0)
      (let ((r (* 3.5 card-height)))
	(loop for alpha from (* 2 pi) downto 0 by (/ pi 36)
	   do (let ((x (* r (cos alpha)))
		    (z (* r (sin alpha))))
		(gl:vertex (- x) 0 (- z))))))
    (gl:color 1 1 1)
    (gl:with-primitives :points
      (gl:vertex 0 0 0)))

(defun rotate-to-player-view (direction)
  (declare (optimize speed))
  (gl:rotate 
   (ecase direction
     (:self 0)
     (:left (- (/ 360 3)))
     (:right (/ 360 3))
     (:skat0 10)
     (:skat1 0))
   0 1 0))

(defun lay-down ()
  "Rotates so the cards will be drawn lying on the table"
  (declare (optimize speed))
  (gl:rotate 270 1 0 0))

(defun flip-cards ()
  "Rotates so the cards will be drawn in a way that the player
would see the other face than before"
  (declare (optimize speed))
  (gl:rotate 180 1 0 0))

(defmethod draw-middle-stack ((module cards))
  "Draws the cards in the middle of the table"
  (declare (optimize speed))
  (with-pushed-matrix
    :modelview
    (let ((dy 0.01))
      (dolist (card (slot-value module 'middle-stack))
	(with-pushed-matrix
	  (rotate-to-player-view (ui-card-from card))
	  (gl:rotate 10 0 1 0)		; turn a little further
	  (lay-down)
	  (gl:translate 0 (- (* 1/4 card-height)) 0)	; shift a little
	  (when (ui-card-covered-p card)
	    ;; flip covered cards
	    (flip-cards))
	  (draw-ui-card-here module card
			     9001 nil))
	(gl:translate 0 dy 0)))))

(defmethod draw-tricks ((module cards) cards)
  (declare (optimize speed))
  (with-pushed-matrix
    :modelview
    (lay-down)
    (flip-cards)
    (let ((dy 0.01))
      (dolist (card cards)
	(draw-ui-card-here module card 9002 nil)
	(gl:rotate 10 0 0 1)		; rotate cards
	(gl:translate 0 dy 0)))))

(defmethod draw-last-trick ((module cards))
  "Draws the last trick next to the sending player"
  (declare (optimize speed))
  (destructuring-bind (direction &rest cards) (last-trick module)
    (with-pushed-matrix
      :modelview
      (rotate-to-player-view direction)
      ;; rotation offset: this trick to the left of the right player
      ;; else to the left and not hidden by our own cards
      (gl:rotate (ecase direction
		   (:self 45)
		   (:right -40)
		   (:left 40)) 0 1 0)
      (gl:translate 0 0 (* 2.4 card-height))
      (when (eq direction :self)
	;; rotate in-place when these are our own cards
	(gl:rotate -20 0 1 0))
      (lay-down)
      (let ((dy 0.01))
	(dolist (card cards)
	  (with-pushed-matrix
	    :modelview
	    (gl:translate 0 (* 1/2 card-height) 0)
	    (draw-ui-card-here module card 9003 nil))
	  (gl:rotate -30 0 0 1)
	  (gl:translate 0 dy 0))))))

(defun draw-tricks-here (module direction cards)
  (declare (optimize speed))
  (with-pushed-matrix
    :modelview
    (rotate-to-player-view direction)
    (gl:rotate 40 0 1 0)	; tricks next to cards
    (gl:translate 0 0 (* 2.5 card-height))
    (draw-tricks module cards))
  (values))

(defun draw-hand-here (module direction cards selection-name-fn)
  (declare (optimize speed))
  (with-pushed-matrix
    :modelview
    (rotate-to-player-view direction)
    (gl:translate 0 (* 1/3 1.2 3.3 card-height) (* 1/3 1.2 3 card-height))
    (gl:rotate -39 1 0 0) ; -49 1 0 0 would be perpendicular to viewer
    (gl:translate 0 (* -1 card-height) 0)
    (gl:rotate 10 0 1 0)
    (draw-hand module cards selection-name-fn))
  (values))

(defmethod draw ((module cards))
  "Zeichnet die Karten"
  (declare (optimize speed))
  (draw-table)
  (gl:enable :texture-2d)
  (gl:enable :alpha-test)
  (gl:tex-env :texture-env :texture-env-mode :modulate)
  (gl:alpha-func :greater 0.1)
  (gl:color 1 1 1)
  ;; tricks
  (map nil (curry #'draw-tricks-here module)
       (list :left :right :self)
       (with-slots (own-tricks left-tricks right-tricks) module
	 (list left-tricks right-tricks own-tricks)))
  ;; other players' cards
  (map nil (rcurry (curry #'draw-hand-here module) (constantly 9000))
       (list :left :right)
       (with-slots (left-cards right-cards) module
	 (list left-cards right-cards)))
  ;; middle stack
  (draw-middle-stack module)
  ;; last trick
  (when (show-last-trick-p module)
    (draw-last-trick module))
  ;; own cards
  (with-selname 1000
    (draw-hand-here module :self (cards module) #'own-card-selname))
  (gl:disable :alpha-test)
  (gl:disable :texture-2d))

;; convenience functions

(defmethod game-starts ((module cards))
  "Removes any cards, clears the middle stack and the trick stacks"
  (setf (cards module) nil)
  (clear-middle module)
  (hide-last-trick module)
  (setf (last-trick module) nil)
  (with-slots (left-tricks right-tricks own-tricks
			   left-cards right-cards) module
    (setf left-tricks nil
	  left-cards nil
	  right-cards nil
	  right-tricks nil
	  own-tricks nil)))

(defmethod leave ((module cards))
  "Delete all cards"
  (with-slots ()
      module
    (game-starts module)))	  ; wrong syntax but correct semantics

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

(define-delayed choose-card ((module cards)) 100
  "Enables the handling of clicks on the cards"
  (if (timeout-scheduled-p module :trick-push)
      ;; wait until trick is gone
      (choose-card module)
      ;; now do it
      (setf (choose-card-p module) t)))

(defmethod select-card ((module cards) x y)
  "Does a selection at P(x,y) and returns the card that the clicked object represents"
  (declare (optimize debug))
  (let ((hit-records (select (ui module) x y)))
    (declare (optimize debug))
    (when hit-records
      (let ((record (dolist (r hit-records) ; look for a card hit
		      (declare (optimize debug))
		      (when (> (length (hit-record-names-on-stack r)) 0)
			(when (= +own-cards+ (car (hit-record-names-on-stack r))) ; card hit
			  (return r))))))
	(declare (optimize debug))
	(when record
	  (let ((nth-card (1- (- (second (hit-record-names-on-stack record)) +own-cards+)))) ; offset
	    (nth nth-card (cards module))))))))

(define-delayed middle-stack-push ((module cards) card direction) 1 ; immediately if possible
  "Pushes another card onto the the stack in the middle of the table"
  (if (timeout-scheduled-p module :trick-push)
      ;; trick is still shown, try again later
      (delay-middle-stack-push 1000 module card direction)
      ;; trick-push timeout timed out, execute now
      (with-slots (middle-stack) module
	(setf middle-stack (nconc middle-stack (list (make-ui-card :from direction :card card)))))))

(defmethod send-card ((module cards) ui-card)
  "Sends the card to the kernel to play it, so also remove it from the hand and
prohibit further reaction on clicks on the cards"
  (setf (choose-card-p module) nil)
  (remove-cards module (list ui-card))
  (middle-stack-push module (ui-card-card ui-card) :self)
  (play-card (ui module) (ui-card-card ui-card)))

(defmethod skat-in-the-middle ((module cards))
  "Places the two skat cards in the middle"
  (dotimes (n 2)
    (push (make-ui-card :covered-p t :from (intern (format nil "SKAT~a" n) :keyword))
	  (slot-value module 'middle-stack))))

(defmethod clear-middle ((module cards))
  "Removes the cards lying in the middle of the table"
  (setf (slot-value module 'middle-stack) nil))

(defmethod card-played ((module cards) from-direction card)
  "Pushes the card onto the middle stack."
  ;; and put the card in the middle
  (middle-stack-push module card from-direction)
  ;; remove a card from the other player's hand
  (pop (slot-value module (ecase from-direction
			    (:left 'left-cards)
			    (:right 'right-cards)))))

(defmethod add-other-players-cards ((module cards) left-or-right n)
  "Pushes n covered cards to an other player's hand"
  (dotimes (n n)
    (push (make-ui-card :covered-p t)
	  (slot-value module (ecase left-or-right
			       (:left 'left-cards)
			       (:right 'right-cards))))))

(define-delayed trick-push ((module cards) direction) 1000
  "Pushes the cards from the middle stack to the tricks of the player"
  (hide-last-trick module)
  (with-slots (own-tricks left-tricks right-tricks middle-stack)
      module
    ;; remember this trick as the latest trick
    (setf (last-trick module) (cons direction (mapcar #'copy-ui-card middle-stack)))
    ;; flip the cards in the middle
    (dolist (ui-card middle-stack)
      (setf (ui-card-covered-p ui-card) t))
    ;; push the trick cards to trick stack
    (ecase direction
      (:self (setf own-tricks (nconc own-tricks middle-stack)))
      (:left (setf left-tricks (nconc left-tricks middle-stack)))
      (:right (setf right-tricks (nconc right-tricks middle-stack))))
    ;; and clear the table
    (clear-middle module)))

(define-delayed trick-to ((module cards) direction) 1
  "Waits until all cards are on the table (middle-stack-push) and
pushes the trick away (trick-push) afterwards"
  (if (timeout-scheduled-p module :middle-stack-push)
      (delay-trick-to 100 module direction)
      (trick-push module direction)))

(defmethod hide-last-trick ((module cards))
  "Hides the last trick"
  (setf (show-last-trick-p module) nil))

(defmethod show-last-trick ((module cards))
  "Display the last trick"
  (setf (show-last-trick-p module) t))	

;; Module methods

(defmethod handle-event ((module cards) event)
  (case-event event
    (:mouse-button-down-event (:x x :y y)
			      (hide-last-trick module)
			      (unless (modal-windows-visible-p)
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
					   (setf candidate-card card))))))))
    (:mouse-motion-event (:x x :y y)
			 (declare (optimize speed))
			 ;; save mouse pos
			 (with-slots (last-mouse-pos) module
			   (setf (aref last-mouse-pos 0) x
				 (aref last-mouse-pos 1) y)))
    (:mouse-button-up-event (:x x :y y)
			    (unless (modal-windows-visible-p)
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
					   (slot-makunbound module 'candidate-card)))))))))

(defmethod timeout-callback ((module cards) do-method timeout-ident)
  (declare (optimize debug))
  (restart-case
      (let* ((queued-top (pop (queued-args module timeout-ident)))
	     (elapsed (ag:get-ticks))
	     (args (cdr queued-top))) ; args for funcall
	(apply do-method args)
	(let ((rest (queued-args module timeout-ident)))
	  (when (and rest (> (- (caar rest) elapsed) 3000))
	    (warn "~a deferred more than 3 sec (~s ticks)!"
		  timeout-ident
		  (- (caar rest) elapsed))
	    (break))
	  (if rest
	      ;; there are more of these timeouts to come
	      (max (- (caar rest) ; this is the next ival
		      elapsed)
		   1) ; but at least 1 tick!
	      ;; else don't reschedule
	      0)))
    (continue () :report "Return from timeout callback without rescheduling"
	      0)))

(defmacro delayed-timeouts (module)
  `(list
    ,@(loop for methodname in *delayed-methods*
	 appending (let ((do-method (conc-symbols 'do- methodname))
			 (timeout-ident (to-keyword methodname)))
		     `(,timeout-ident
		       (let ((timeout (alloc-finalized ,module 'ag:timeout)))
			 (ag:set-timeout timeout
					 (lambda-timeout-callback (obj ival arg)
					   (declare (ignore obj ival arg) (optimize debug))
					   (timeout-callback ,module #',do-method ,timeout-ident))
					 (null-pointer) nil)
			 timeout))))))

(defmacro make-queued-actions-list ()
  `(list ,@(loop for methodname in *delayed-methods*
	      collect `(cons ,(to-keyword methodname) nil))))

(defmethod setup-timeouts ((module cards))
  (setf (timeouts module) (delayed-timeouts module)
	(queued-actions module) (make-queued-actions-list)))

(defmethod initialize-instance :after ((module cards) &key)
  "Loads the card textures and defines callbacks for this module"
  (load-textures module)
  (create-display-lists module)
  (setup-timeouts module))

(defmethod cleanup ((module cards))
  (gl:delete-textures (loop for elm in (textures module)
			 when (numberp elm) collect elm)))

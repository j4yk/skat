(in-package gl-ui)

(defclass bidding-window (agar-window) nil)

(defmethod move-left ((w bidding-window))
  (ag:set-window-geometry (window w) 100 (round (/ 480 3)) -1 -1))

(defmethod move-right ((w bidding-window))
  (ag:set-window-geometry (window w) (- 640 180) (round (/ 480 3)) -1 -1))


;; someone bids

(defclass bidder-window (bidding-window)
  ((bid-value-label) (bid-value-fv)
   (bid-value :accessor bid-value)
   (passed-label)))

(defmethod initialize-instance :after ((w bidder-window) &key)
  (let*-slots w
      ((bid-value-fv (make-foreign-variable :ptr (alloc-finalized w :int)))
       (window (ag:window-new :notitle :noborders))
       (bid-value-label (expanded (ag:new-polled-label window nil "Sagt: %i" (foreign-variable-ptr bid-value-fv))))
       (passed-label (ag:label-new-string (null-pointer) "Passt")))
    (setf (bid-value w) 18)
    (ag:set-window-padding window 10 10 10 10)
    (ag:size-hint-label bid-value-label 1 "Sagt: 256")))

(defmethod (setf bid-value) :after (value (w bidder-window))
  (with-slots (bid-value-fv) w
    (setf (cffi:mem-aref (foreign-variable-ptr bid-value-fv) :int) value)))

(defmethod bid-received ((w bidder-window) sender-address value)
  (declare (ignore sender-address))
  (show w)
  (with-slots (window passed-label bid-value-label) w
    (ensure-detached passed-label window)
    (ensure-attached bid-value-label window))
  (setf (bid-value w) value))

(defmethod pass-received ((w bidder-window) sender-address value)
  (declare (ignore sender-address value))
  (show w)
  (with-slots (window passed-label bid-value-label) w
    (ensure-detached bid-value-label window)
    (ensure-attached passed-label window)))


;; someone listens

(defclass listener-window (bidding-window)
  ((action-label) (action-fv)
   (action :accessor action)))

(defmethod initialize-instance :after ((w listener-window) &key)
  (let*-slots w
      ((action-fv (make-foreign-variable :ptr (alloc-finalized w :char :count 15) :size 15))
       (window (ag:window-new :notitle :noborders))
       (action-label (expanded (ag:new-polled-label window nil "%s" (foreign-variable-ptr action-fv)))))
    (setf (action w) "Wartet...")
    (ag:set-window-padding window 10 10 10 10)
    (ag:size-hint-label action-label 1 "Denkt nach...")))

(defmethod (setf action) (value (w listener-window))
  (with-slots (action-fv) w
    (lisp-string-to-foreign value (foreign-variable-ptr action-fv) (foreign-variable-size action-fv))))

(defmethod bid-received ((w listener-window) sender-address value)
  (declare (ignore sender-address value))
  (show w)
  (setf (action w) "Denkt nach..."))

(defmethod join-received ((w listener-window) sender-address value)
  (declare (ignore sender-address value))
  (show w)
  (setf (action w) "Ja"))

(defmethod pass-received ((w listener-window) sender-address value)
  (declare (ignore sender-address value))
  (show w)
  (setf (action w) "Passt"))


;; bid yourself

(defclass own-bidder-window (bidding-window)
  ((client-hbox) (bid-value-ucombo) (bid-button)
   (pass-button)
   (selected-bid-value-fv)))

(defmethod initialize-instance :after ((w own-bidder-window) &key)
  (let*-slots w
      ((selected-bid-value-fv (make-foreign-variable :ptr (alloc-finalized w :pointer)))
       (window (ag:window-new :nobuttons))
       (client-hbox (ag:hbox-new window))
       (bid-value-ucombo
       	(expanded (ag:new-polled-ucombo client-hbox nil
					(lambda-event-handler event
					  (let ((tlist (ag:event-self event)))
					    (fill-tlist-with-game-point-levels
					     (module w) tlist))) "")))
       (bid-button (ag:button-new-fn client-hbox nil "Ansagen"
				     (lambda-event-handler event
				       (declare (ignore event))
				       (bid w)) ""))
       (pass-button (ag:button-new-fn client-hbox nil "Passen"
				      (lambda-event-handler event
					(declare (ignore event))
					(pass w)) "")))
    (ag:window-set-caption window "Reizwert ansagen")
    ;; insert a dummy label to make the window a little bit bigger by default
    (let ((dummy-label (ag:label-new-string client-hbox "XXX")))
      (ag:hide-widget dummy-label))
    (ag:set-event bid-value-ucombo "ucombo-selected" (lambda-event-handler event
						       (declare (ignore event))
						       (ag:window-update window)) "")
    ;; bind the pointer of the selected tlist item to selected-bid-value-fv
    ;; #'fill-tlist-with-game-point-levels will store the actual integer
    ;; typecasted as a pointer in each tlist-item
    (ag:bind-pointer (ag:ucombo-list bid-value-ucombo)
    		     "selected" (foreign-variable-ptr selected-bid-value-fv))))

(defmethod selected-bid-value ((w own-bidder-window))
  (with-slots (selected-bid-value-fv) w
    (cffi:mem-aref (foreign-variable-ptr selected-bid-value-fv) :int)))

(defmethod (setf selected-bid-value) (value (w own-bidder-window))
  (with-slots (bid-value-ucombo) w
    (ag:post-event (null-pointer) (ag:ucombo-list bid-value-ucombo) "tlist-poll" "")
    (ag:tlist-select-ptr (ag:ucombo-list bid-value-ucombo) (cffi:make-pointer value))))

(defmethod bid ((w own-bidder-window))
  (send-bid (module w) (selected-bid-value w)))

(defmethod pass ((w own-bidder-window))
  (send-pass (module w) (selected-bid-value w)))

(defmethod start-bidding ((w own-bidder-window) listener min-value)
  (declare (ignore listener))
  (setf (selected-bid-value w) min-value)
  (show w))

;; bidding module

(defclass bidding (module)
  ((left-player :accessor left-player)
   (right-player :accessor right-player)
   (listener :accessor listener)
   (bidder :accessor bidder)
   (listener-p :accessor listener-p :initform nil)
   (bidder-window :accessor bidder-window)
   (listener-window :accessor listener-window)
   (own-bidder-window :accessor own-bidder-window)
   (own-listener-window :accessor own-listener-window)
   (game-point-levels :accessor game-point-levels :initform kern:*game-point-levels*)))

(defmethod initialize-instance :after ((module bidding) &key)
  (setf (bidder-window module) (make-instance 'bidder-window :module module))
  (setf (listener-window module) (make-instance 'listener-window :module module))
  (setf (own-bidder-window module) (make-instance 'own-bidder-window :module module)))

(defmethod cleanup ((module bidding))
  (map nil #'(lambda (w) (hide w) (ag:detach-object (window w)))
       (list (bidder-window module) (listener-window module)
	     (own-bidder-window module))))

(defmethod fill-tlist-with-game-point-levels ((module bidding) tlist)
  "Fills an agar:tlist with items. The items are labelled with the game point level values
from (game-point-levels module) and the each item's user specified pointer will have
the numerical value of that game point level (i. e. it is not really a pointer!)"
  (ag:tlist-begin tlist)
  (dolist (n (game-point-levels module))
    (ag:tlist-add-ptr tlist (null-pointer) (format nil "~a" n) (cffi:make-pointer n)))
  (ag:tlist-end tlist))

(defmethod introduce-playmates ((module bidding) left-player-address right-player-address)
  "Remember which player is left and which is right"
  (setf (left-player module) left-player-address)
  (setf (right-player module) right-player-address))

(defmethod reset-game-point-levels ((module bidding))
  "Reset the bidding values to the start (18 first)"
  (setf (game-point-levels module) kern:*game-point-levels*))

(defmethod bid-received ((module bidding) sender value)
  "Moves the bidding and listening windows accordingly and sets their labels"
  (setf (bidder module) sender)
  ;; check the validness of this bid
  (assert (>= value (first (game-point-levels module)))
	  (value (game-point-levels module)) "Received bidding value is too low")
  ;; cut the list of possible bidding values
  (kern:cut-away-game-point-levels value (game-point-levels module))
  ;; update bidder-window
  (bid-received (bidder-window module) sender value)
  ;; move the windows according to the roles
  (if (listener-p module)
      (if (equal sender (left-player module))
	  (move-left (bidder-window module))
	  (move-right (bidder-window module)))
      (progn
	;; update the listener-window
	(bid-received (listener-window module) sender value)
	(if (equal sender (left-player module))
	    (progn (setf (listener module) (right-player module))
		   (move-left (bidder-window module))
		   (move-right (listener-window module)))
	    (progn (setf (listener module) (left-player module))
		   (move-right (bidder-window module))
		   (move-left (listener-window module)))))))

(defmethod join-received ((module bidding) sender-address value)
  ;; next bidding value must be higher
  (setf (game-point-levels module) (cdr (game-point-levels module)))
  ;; update the listener-window
  (join-received (listener-window module) sender-address value))

(defmethod pass-received ((module bidding) sender-address value)
  (pass-received (if (equal sender-address (bidder module))
		     (bidder-window module)
		     (listener-window module)) sender-address value))

(defmethod listen-to ((module bidding) bidder-address)
  "Hide listener window"
  (setf (listener-p module) t)
  (hide (listener-window module)))

(defmethod start-bidding ((module bidding) listener-address min-value)
  "Show own bidding window"
  (setf (game-point-levels module)
	(kern:cut-away-game-point-levels min-value (game-point-levels module)))
  (start-bidding (own-bidder-window module) listener-address min-value))

(defmethod send-bid ((module bidding) value)
  "Sends a bid request to the kernel"
  (hide (own-bidder-window module))
  (call-kernel-handler (ui module) 'bid value))

(defmethod send-join ((module bidding) value)
  "Sends a join request to the kernel"
  (call-kernel-handler (ui module) 'join value))

(defmethod send-pass ((module bidding) value)
  "Sends a pass request to the kernel"
  (hide (own-bidder-window module))
  (call-kernel-handler (ui module) 'pass value))
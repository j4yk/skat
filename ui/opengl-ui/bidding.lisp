(in-package gl-ui)

(defclass bidding-window (agar-window) nil)

(defmethod move-left ((w bidding-window))
  (ag:set-window-geometry (window w) 100 (round (/ 480 3)) -1 -1))

(defmethod move-right ((w bidding-window))
  (ag:set-window-geometry (window w) (- 640 180) (round (/ 480 3)) -1 -1))


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

      
(defclass bidding (module)
  ((left-player :accessor left-player)
   (right-player :accessor right-player)
   (listener :accessor listener)
   (bidder :accessor bidder)
   (listener-p :accessor listener-p :initform nil)
   (bidder-window :accessor bidder-window)
   (listener-window :accessor listener-window)))

(defmethod initialize-instance :after ((module bidding) &key)
  (setf (bidder-window module) (make-instance 'bidder-window :module module))
  (setf (listener-window module) (make-instance 'listener-window :module module)))

(defmethod introduce-playmates ((module bidding) left-player-address right-player-address)
  "Remember which player is left and which is right"
  (setf (left-player module) left-player-address)
  (setf (right-player module) right-player-address))

(defmethod bid-received ((module bidding) sender value)
  "Moves the bidding and listening windows accordingly and sets their labels"
  (setf (bidder module) sender)
  (bid-received (bidder-window module) sender value)
  (if (listener-p module)
      (if (equal sender (left-player module))
	  (move-left (bidder-window module))
	  (move-right (bidder-window module)))
      (progn
	(bid-received (listener-window module) sender value)
	(if (equal sender (left-player module))
	    (progn (setf (listener module) (right-player module))
		   (move-left (bidder-window module))
		   (move-right (listener-window module)))
	    (progn (setf (listener module) (left-player module))
		   (move-right (bidder-window module))
		   (move-left (listener-window module)))))))

(defmethod join-received ((module bidding) sender-address value)
  (join-received (listener-window module) sender-address value))

(defmethod pass-received ((module bidding) sender-address value)
  (pass-received (if (equal sender-address (bidder module))
		     (bidder-window module)
		     (listener-window module)) sender-address value))

(defmethod listen-to ((module bidding) bidder-address)
  "Hide listener window"
  (setf (listener-p module) t)
  (hide (listener-window module)))

(defmethod cleanup ((module bidding))
  (map nil #'(lambda (w) (hide w) (ag:detach-object (window w)))
       (list (bidder-window module) (listener-window module))))

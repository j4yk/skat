(in-package gl-ui)

(defclass player-info-window (agar-window)
  ((client-vbox)
   (name-label) (name :accessor name :initarg :player-name) (name-fv)
   (score-label) (score :accessor score :initform 0) (score-fv)
   (bid-value-label) (bid-value :accessor bid-value :initform 0) (bid-value-fv)
   (role-label) (role :accessor role :initform "") (role-fv)
   (additional-info-label) (additional-info :accessor additional-info :initform "") (additional-info-fv)))

(defmethod initialize-instance :after ((w player-info-window) &key player-name &allow-other-keys)
  (let*-slots w
      (;; foreign memory references first
       (name-fv (make-foreign-variable :ptr (if player-name
						(alloc-finalized-string w player-name)
						(alloc-finalized w :char :count 1024))
				       :size (if player-name
						 (cffi:with-foreign-string ((ptr size) player-name)
						   size) ; have the size calculated in the foreign land
						 1024)))
       (score-fv (make-foreign-variable :ptr (alloc-finalized w :int)))
       (bid-value-fv (make-foreign-variable :ptr (alloc-finalized w :int)))
       (role-fv (make-foreign-variable :ptr (alloc-finalized w :char :count 20) :size 20))
       (additional-info-fv (make-foreign-variable :ptr (alloc-finalized w :char :count 100) :size 100))
       ;; and now the widgets
       (window (ag:window-new :notitle :noborders :nomove))
       (client-vbox (expanded (ag:vbox-new window)))
       (name-label (ag:new-polled-label client-vbox nil "%s" (foreign-variable-ptr name-fv)))
       (score-label (ag:new-polled-label client-vbox nil "Punkte: %i" (foreign-variable-ptr score-fv)))
       (bid-value-label (ag:new-polled-label client-vbox nil "Gereizt: %i" (foreign-variable-ptr bid-value-fv)))
       (role-label (ag:new-polled-label client-vbox nil "%s" (foreign-variable-ptr role-fv)))
       (additional-info-label (ag:new-polled-label client-vbox nil "%s" (foreign-variable-ptr additional-info-fv))))
    ;; setze Startwerte, damit kein Speichermüll angezeigt wird
    (setf (score w) 0
	  (bid-value w) 0
	  (role w) ""
	  (additional-info w) "")
    ;; und gib size-hints, damit die Fenster groß genug werden
    (ag:size-hint-label role-label 1 "Alleinspieler")
    (ag:size-hint-label additional-info-label 1 (format nil "Grand Hand Schneider"))))

(defmethod (setf name) :after (name (w player-info-window))
  "Copy the new name to foreign memory so the label gets updated."
  (with-slots (name-fv) w
    (lisp-string-to-foreign name (foreign-variable-ptr name-fv) (foreign-variable-size name-fv))))

(defmethod (setf score) :after (score (w player-info-window))
  "Copy the score value to foreign memory so the label gets updated."
  (with-slots (score-fv) w
    (setf (cffi:mem-aref (foreign-variable-ptr score-fv) :int) score)))

(defmethod (setf bid-value) :after (bid-value (w player-info-window))
  "Copy the bid value to foreign memory so the label gets updated."
  (with-slots (bid-value-fv) w
    (setf (cffi:mem-aref (foreign-variable-ptr bid-value-fv) :int) bid-value)))

(defmethod (setf role) :after (role (w player-info-window))
  "Copy the role name to foreign memory so the label gets updated."
  (with-slots (role-fv) w
    (lisp-string-to-foreign role (foreign-variable-ptr role-fv) (foreign-variable-size role-fv))))

(defmethod (setf additional-info) :after (additional-info (w player-info-window))
  "Copy the additional info string to foreign memory so the label gets updated."
  (with-slots (additional-info-fv) w
    (lisp-string-to-foreign additional-info (foreign-variable-ptr additional-info-fv)
			    (foreign-variable-size additional-info-fv))))


;; Players module

(defclass players (module)
  ((left-player-name) (right-player-name)
   (own-address :documentation "This is the own player's address")
   (declarer-address :accessor declarer-address)
   (left-player-window)
   (right-player-window)
   (own-player-window)))

(defmethod cleanup ((module players))
  (with-slots (left-player-window right-player-window own-player-window) module
    (when (slot-boundp module 'left-player-window)
      (hide left-player-window)
      (ag:detach-object (window left-player-window))
      (slot-makunbound module 'left-player-window)
    (when (slot-boundp module 'right-player-window)
      (hide right-player-window)
      (ag:detach-object (window right-player-window))
      (slot-makunbound module 'right-player-window))
    (when (slot-boundp module 'own-player-window)
      (hide own-player-window)
      (ag:detach-object (window own-player-window))
      (slot-makunbound module 'own-player-window)))))

(defun check-slot-unbound (object slotname)
  (when (slot-boundp object slotname)
    (error "~s of ~s already bound!" slotname object)))

(defmethod own-address ((module players) address)
  (let*-slots module ((own-address address))))

(defmethod introduce-playmates ((module players) name-left name-right)
  "Remember their names"
  (let*-slots module
      ((left-player-name name-left)
       (right-player-name name-right))))

(defmethod show-playmates ((module players))
  "Initializes and shows the player info windows"
  (assert (slot-boundp module 'left-player-name))
  (assert (slot-boundp module 'right-player-name))
  (check-slot-unbound module 'left-player-window)
  (check-slot-unbound module 'right-player-window)
  (with-slots (left-player-name right-player-name own-address) module
    (let*-slots module
	((left-player-window (make-instance 'player-info-window :player-name left-player-name :module module))
	 (right-player-window (make-instance 'player-info-window :player-name right-player-name :module module))
	 (own-player-window (make-instance 'player-info-window :player-name own-address :module module)))
      (ag:window-set-position (window left-player-window) :tl nil)
      (ag:window-set-position (window right-player-window) :tr nil)
      (ag:window-set-position (window own-player-window) :bl nil)
      (mapcar #'show (list left-player-window right-player-window own-player-window)))))

(defmethod declarer ((module players) declarer-address)
  (with-slots (own-address left-player-name right-player-name
			   own-player-window left-player-window right-player-window)
      module
    (setf (declarer-address module) declarer-address)
    (cond ((equal declarer-address own-address)
	   (setf (role own-player-window) "Alleinspieler"
		 (role left-player-window) "Gegenspieler"
		 (role right-player-window) "Gegenspieler"))
	  ((equal declarer-address left-player-name)
	   (setf (role own-player-window) "Gegenspieler"
		 (role left-player-window) "Alleinspieler")
		 (role right-player-window) "Gegenspieler")
	  ((equal declarer-address right-player-name)
	   (setf (role own-player-window) "Gegenspieler"
		 (role left-player-window) "Gegenspieler"
		 (role right-player-window) "Alleinspieler")))))

(defmethod player-name ((module players) player-address)
  (with-slots (own-address left-player-name right-player-name)
      module
    (cond ((equal player-address own-address) own-address)
	  ((equal player-address left-player-name) left-player-name)
	  ((equal player-address right-player-name) right-player-name))))

(defmethod self-p ((module players) address)
  "Returns t if address is the player's own address"
  (with-slots (own-address) module
    (equal address own-address)))

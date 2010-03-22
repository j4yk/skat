(in-package gl-ui)

;; these enums are matched with integers selected in an AG_Radio

(cffi:defcenum suit
  :diamonds :hearts :spades :clubs :grand :null)

(cffi:defcenum null-add
  :standard :ouvert)

(cffi:defcenum suit-add
  :hand :schneider :schwarz :ouvert)

(defclass select-declaration-window (agar-window)
  ((hand-p :accessor hand-p :initarg :hand :initform nil)
   (suit-fv) (null-add-fv) (suit-add-fv)
   (client-vbox) (client-hbox) (left-vbox) (suit-radio)
   (right-vbox) (none-possible-label) (null-add-radio) (suit-add-radio)
   (declare-btn)))

(defmethod selected-suit ((w select-declaration-window))
  "Current selection in the trump suit selection radio"
  (mem-aref (foreign-variable-ptr (slot-value w 'suit-fv)) 'suit))

(defmethod suit-changed ((w select-declaration-window))
  "Ensures the correct widgets are shown on the right of the window"
  (with-slots (right-vbox none-possible-label
			  null-add-radio suit-add-radio
			  hand-p)
      w
    (case (selected-suit w)
      (:null (ensure-detached none-possible-label right-vbox)
	     (ensure-detached suit-add-radio right-vbox)
	     (ensure-attached null-add-radio right-vbox))
      (otherwise (ensure-detached null-add-radio right-vbox)
		 (ensure-detached (if hand-p
				      none-possible-label
				      suit-add-radio)
				  right-vbox)
		 (ensure-attached (if hand-p
				      suit-add-radio
				      none-possible-label)
				  right-vbox)))
    (ag:window-update (window w))))

(defmethod (setf selected-suit) (value (w select-declaration-window))
  (setf (mem-aref (foreign-variable-ptr (slot-value w 'suit-fv)) 'suit)
	value)
  (suit-changed w))

(defmethod selected-declarations ((w select-declaration-window))
  "Returns which item is selected on the right of the window"
  (case (selected-suit w)
    (:null (mem-aref (foreign-variable-ptr (slot-value w 'null-add-fv)) 'null-add))
    (otherwise (mem-aref (foreign-variable-ptr (slot-value w 'suit-add-fv)) 'suit-add))))

(defmethod declaration-list ((w select-declaration-window))
  "Returns the ready to be sent list of declarations"
  (let ((selected-decls (selected-declarations w))
	(default-decl (if (hand-p w) (list :hand) nil)))
    (nconc (list (selected-suit w)) ; suit
	   default-decl		   ; hand
	   (case (selected-suit w)
	     (:null		      ; :ouvert or nothing
	      (when (eq selected-decls :ouvert) (list :ouvert)))
	     (otherwise		; schneider, schwarz, ouvert
	      ;; always include the lower ones
	      (case selected-decls
		(:ouvert (list :declared-schneider :declared-schwarz :ouvert))
		(:schwarz (list :declared-schneider :declared-schwarz))
		(:schneider (list :declared-schneider))
		(otherwise nil))))))) ; don't append :hand a second time

(defmethod send-it ((w select-declaration-window))
  "Hides the window and sends the declaration to the kernel"
  (hide w)
  (call-kernel-handler
   (ui (module w)) 'declaration (declaration-list w)))

(defmethod initialize-instance :after ((w select-declaration-window) &key)
  "Instanciates all possible widgets for the window, including the
ones that aren't initially shown"
  (let ((none-possible-text (format nil "Da der Skat~%aufgenommen~%wurde, sind~%keine zusätz-~%lichen~%Ansagen~%möglich.")))
    (let*-slots w
	((suit-fv (make-foreign-variable :ptr (alloc-finalized w 'suit)))
	 (null-add-fv (make-foreign-variable :ptr (alloc-finalized w 'null-add)))
	 (suit-add-fv (make-foreign-variable :ptr (alloc-finalized w 'suit-add)))
	 (window (ag:window-new :modal :nobuttons :nohresize :novresize))
	 (client-vbox (expanded (ag:vbox-new window)))
	 (client-hbox (expanded (ag:hbox-new client-vbox)))
	 (left-vbox (expanded-v (ag:vbox-new client-hbox)))
	 (suit-radio (expanded-v (ag:new-radio-int left-vbox nil
						   (list "Karo" "Herz" "Pik" "Kreuz" "Grand" "Null")
						   (foreign-variable-ptr suit-fv))))
	 (right-vbox (expanded-v (ag:vbox-new client-hbox)))
	 (none-possible-label
	  (expanded-h
	   (ag:new-label (null-pointer) nil
			 none-possible-text)))
	 (null-add-radio (expanded-v (ag:new-radio-int (null-pointer) nil
						       (list "Normal" "Ouvert")
						       (foreign-variable-ptr null-add-fv))))
	 (suit-add-radio (expanded-v (ag:new-radio-int (null-pointer) nil
						       (list "Hand" "Schneider" "Schwarz" "Ouvert")
						       (foreign-variable-ptr suit-add-fv))))
	 (declare-btn
	  (expanded-h
	   (ag:button-new-fn
	    client-vbox nil "Ansagen"
	    (std-event-handler (send-it w)) ""))))
      (ag:window-set-caption window "Ansage")
      ;; default selected radio buttons
      (setf (mem-aref (foreign-variable-ptr suit-add-fv) 'suit-add) :hand)
      (setf (mem-aref (foreign-variable-ptr null-add-fv) 'null-add) :standard)
      (setf (selected-suit w) :diamonds)
      ;; have suit-changed called on selection change
      (ag:set-radio-changed-event suit-radio (std-event-handler (suit-changed w)))
      ;; size hint for the fat label
      (ag:size-hint-label none-possible-label 7 none-possible-text))))

(defmethod show ((w select-declaration-window))
  "Show the window, but intially show it (to Agar) with the
none-possible-label because this is supposed to take up the most
space"
  (let ((suit (selected-suit w))
	(hand-p (hand-p w)))
    (setf (hand-p w) nil)
    (setf (selected-suit w) :diamonds)
    (call-next-method)
    (setf (hand-p w) hand-p)
    (setf (selected-suit w) suit)))


;; Module

(defclass game-declaration (module)
  ())

(defmethod query-hand ((module game-declaration))
  "Prompts the user whether he wants to take the skat"
  (ag:prompt-options "Skat aufnehmen?"
		     (list "Aufnehmen"
			   (lambda-event-handler event
			     (take-skat (ui module))
			     (get-rid-of-window (ag:event-ptr event 1))))
		     (list "Hand spielen"
			   (lambda-event-handler event
			     (play-hand (ui module))
			     (get-rid-of-window (ag:event-ptr event 1)))))
  (values))

(defmethod try-send-skat ((module game-declaration) skat-send-window)
  "Tries to send the skat, if a wrong number of cards was selected,
show an error dialog and don't close the send button window"
  (handler-case
      (progn
	(send-skat (ui module))
	(get-rid-of-window skat-send-window))
    (wrong-number-of-cards-error ()
      (ag:text-msg :error "Du musst genau zwei Karten in den Skat drücken!"))))

(defmethod query-skat ((module game-declaration))
  "Displays the button to eventually send the two skat cards"
  (let* ((window (ag:window-new :notitle :noborders :keepabove))
	 (btn (ag:new-button window nil
			     "Wähle zwei Karten aus und klicke dann hier zum absenden"
			     (std-event-handler
			       (try-send-skat module window)))))
    (declare (ignore btn))
    (ag:window-show window)))

(defmethod query-declaration ((module game-declaration) hand-p)
  "Presents the player the declaration selection window"
  (let ((w (make-instance 'select-declaration-window
			  :hand hand-p :module module)))
    (show w)))

(defmethod show-declarer ((module game-declaration) declarer-address)
  (ag:text-msg :info "~a ist der Alleinspieler dieses Spiels"
	       (player-name (ui module) declarer-address)))

(defmethod announce-declaration ((module game-declaration) declarer declaration)
  (ag:text-msg :info "~a spielt ~a" declarer (declaration-text declaration)))

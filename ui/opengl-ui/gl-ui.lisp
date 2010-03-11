(in-package gl-ui)

(defclass opengl-ui (ui::base-ui)
  ((modules :accessor modules :initform nil :documentation "Liste der aktiven Module")
   (priortized-modules :accessor priortized-modules :initform nil :documentation "Liste der aktiven Module, die Events zuerst verarbeiten dürfen")
   (running-p :reader running-p :initform nil)
   (last-selection :initform nil))
  (:documentation "OpenGL-UI Klasse. Zentrales Objekt für die OpenGL-Schnittstelle"))

;; Strukturspezifisch

(defun find-module (class ui)
  "Returns the first module of given class from (modules ui)"
  (find (find-class class) (modules ui) :key #'class-of))

(defun insert-module (module ui)
  (push module (modules ui)))

(defun remove-module (module ui)
  (setf (modules ui) (delete module (modules ui))))

(defun cleanup-and-remove-module (module ui)
  (cleanup module)
  (remove-module module ui))

(defmacro with-modules ((&rest modules) &body body)
  "Binds the modules to symbols that equal the modules' class names for the lexical context of body.
Meant to be used in request handler functions where 'ui is bound to the ui."
  `(let (,@(loop for module in modules
	      collect `(,module (find-module ',module ui))))
     ,@body))

;; UI specific

(defmethod ui:start ((ui opengl-ui) &optional no-new-thread-p)
  "Startet die OpenGL-Benutzerschnittstelle.
STUB"
  (declare (ignore ui no-new-thread-p))
  (error "Not implemented yet!"))

(defmethod ui:stop ((ui opengl-ui))
  "Hält die OpenGL-Benutzerschnittstelle an.
STUB"
  (declare (ignore ui))
  (error "Not implemented yet!"))

;; request handlers

(defhandler ui:login-struct (opengl-ui struct-classname)
  (let ((module (find-module 'login-and-register ui)))
    (query-login module)))

;; function and control flow

(defmethod player-name ((ui opengl-ui) player-address)
  (with-modules (players)
    (player-name players player-address)))

;; zu Senden: Login-Data

(defhandler ui:own-address (opengl-ui address)
  (with-modules (players)
    (own-address players address)))

(defhandler ui:registration-struct (opengl-ui struct-classname)
  (let ((module (find-module 'login-and-register ui)))
    (query-registration module)))

;; zu Senden: Registration-Data

(defhandler ui:registration-reply (opengl-ui accepted)
  (let ((module (find-module 'login-and-register ui)))
    (if accepted
	(registration-accepted module)
	(registration-denied module))))

(defhandler ui:server-update (opengl-ui events)
  (when (getf events :player-join)
    (player-joined (find-module 'login-and-register ui) (getf events :player-join)))
  (when (getf events :player-leave)
    (player-left (find-module 'login-and-register ui) (getf events :player-leave))))

;; zu Senden: Unregister

(defhandler ui:logout (opengl-ui address)
  #| idle |#)

(defhandler ui:playmates (opengl-ui left right)
  (let ((players (find-module 'players ui))
	(bidding (find-module 'bidding ui)))
    (introduce-playmates bidding left right)
    (introduce-playmates players left right)))

(defhandler ui:game-start (opengl-ui)
  "Host sent game-start"
  (with-modules (login-and-register bidding players)
    (reset-game-point-levels bidding)
    (game-starts login-and-register)
    (show-playmates players)))

(defhandler ui:cards (opengl-ui cards)
  "Called by Kernel when the Host has distrubuted the cards.
Hands the cards over to the cards module."
  (let ((cards-mod (find-module 'cards ui)))
    (add-cards cards-mod cards)))

(defhandler ui:start-bidding (opengl-ui listener min-value)
  (let ((module (find-module 'bidding ui)))
    (start-bidding module listener min-value)))

(defhandler ui:listen (opengl-ui bidder)
  (let ((module (find-module 'bidding ui)))
    (listen-to module bidder)))

(defhandler ui:bid (opengl-ui value)
  (let ((module (find-module 'bidding ui)))
    (bid-received module ui:sender value)))

(defhandler ui:join (opengl-ui value)
  (let ((module (find-module 'bidding ui)))
    (join-received module ui:sender value)))

(defhandler ui:reply-to-bid (opengl-ui value)
  (let ((module (find-module 'bidding ui)))
    (query-join module value)))

(defhandler ui:pass (opengl-ui value)
  (let ((module (find-module 'bidding ui)))
    (pass-received module ui:sender value)))

(defhandler ui:declarer (opengl-ui declarer)
  (with-modules (players notifications game-declaration)
    (declarer players declarer)
    (if (self-p players declarer)
	(query-hand game-declaration)
	(show-declarer notifications declarer))))

(defmethod take-skat ((ui opengl-ui))
  "Send hand-decision to kernel, taking the skat."
  (call-kernel-handler ui 'hand-decision nil))

(defhandler ui:skat (opengl-ui skat)
  "Called by Kernel when the skat is received from the host.
Adds two cards and lets the player select two."
  (assert (= 2 (length skat)))
  (with-modules (cards game-declaration)
    (add-cards cards skat)
    (select-skat cards)
    (query-skat game-declaration)))

(defmethod query-declaration ((ui opengl-ui) hand-p)
  "Calls this generic function on the game-declaration module"
  (with-modules (game-declaration)
    (query-declaration game-declaration hand-p)))

(defmethod send-skat ((ui opengl-ui))
  "Sends the chosen cards back to Kernel and removes the cards from
  the player's hand"
  (with-modules (cards)
    (let ((skat (selected-cards cards)))
      (unless (= 2 (length skat)) (error "Must push two cards into the skat!"))
      (remove-cards cards skat)
      (end-choose-skat cards)		; cleanup
      (call-kernel-handler ui 'skat skat) ; pass it on
      (query-declaration ui nil))))	  ; processed the Skat, so no hand

(defmethod play-hand ((ui opengl-ui))
  "Send hand-decision to kernel, playing a hand game
and presents the player the declaration dialog."
  (call-kernel-handler ui 'hand-decision t)
  (query-declaration ui t))

(defhandler choose-card (opengl-ui)
  "Host hat mitgeteilt, dass man am Stich ist."
  (with-modules (cards)
    (choose-card cards)))

(defmethod play-card ((ui opengl-ui) card)
  "Sendet eine Karte zum Spielen an den Kernel zurück"
  (call-kernel-handler ui 'card card))

(defhandler card (opengl-ui card)
  "Eine Karte wurde von jemandem anders gespielt"
  (with-modules (cards)
    (card-played cards card)))

(defhandler trick (opengl-ui cards winner)
  "Ein Stich wurde zugeteilt"
  (error "not implemented yet"))

(defhandler game-over (opengl-ui prompt)
  "Spiel ist/wurde beendet.
prompt gibt an, ob nach einem neuen Spiel gefragt werden soll"
  (if prompt
      (prompt-for-new-game (error "don't know module"))
      (end-game (error "don't know module"))))

;; rendering

(defun skat-window ()
  "Erstellt das Skat-SDL-Fenster inklusive OpenGL-Kontext."
  (prog1 (sdl:window 640 480 :title-caption "Skat"
		     :fps (make-instance 'sdl:fps-timestep)
		     :flags '(sdl:sdl-opengl sdl:sdl-doublebuf))
    (init-gl 640 480)))

(defun render-non-agar-modules (modules agar-module)
  (dolist (module (remove agar-module modules))
    (draw module)))

(defmethod render-everything ((ui opengl-ui) agar-module)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (if ag:*video-initialized*
      (ag:render
	(with-standard-rendering
	  ;; view perspective
	  ;; Normal:
	  (glu:look-at 0 3 0 0 -3 -15 0 1 0)
	  ;; Draufsicht:
	  ;; (glu:look-at 0 15 -10
	  ;; 	       0 -3 -15
	  ;; 	       0 0 -1)
	  (gl:translate 0 -3 -15)	; go to table center
	  ;; draw all modules with normal GL matrices
	  (render-non-agar-modules (modules ui) agar-module))
	;; draw Agar's world with its own matrices
	(draw agar-module))
      (dolist (module (remove agar-module (modules ui)))
	(draw module))))

;; selection

(defmethod perform-selection ((ui opengl-ui) x y)
  "Performs a selection at \(x|y\) on everything, saves
\(list x y hit-records\) to last-selection slot of ui and
returns sorted hit-records."
  (declare (optimize debug))
  (with-modules (agar)
    (let ((hit-records (sort (select-gl-object x y #'render-non-agar-modules (modules ui) agar)
			     #'< :key #'hit-record-max-z)))
      (let*-slots ui
	  ((last-selection (list x y hit-records)))
	hit-records))))

(defmethod select ((ui opengl-ui) x y)
  "Queries UI for the hit-records resulting from a selection at (x|y)"
  (with-slots (last-selection) ui
    (if last-selection
	(destructuring-bind (sx sy hit-records) ui
	  (if (and (= sx x) (= sy y))
	      hit-records
	      (perform-selection ui x y)))
	(perform-selection ui x y))))

;; main loop

(defun standard-main-loop (ui)
  (when (running-p ui) (return-from standard-main-loop))
  (setf (slot-value ui 'running-p) t)
  (unwind-protect
       (let ((agar-module (find-module 'agar ui)))
	 (defun %used-agar-module () agar-module)
	 (macrolet ((process-event ()
		      `(dolist (module (modules ui))
			 (handle-event module sdl-event))))
	   (labels ((main-loop ()
		      (sdl:with-events (:poll sdl-event)
			(:quit-event () t)
			(:active-event () (process-event))
			(:key-down-event () (process-event))
			(:key-up-event () (process-event))
			(:mouse-motion-event () (process-event))
			(:mouse-button-down-event () (process-event))
			(:mouse-button-up-event () (process-event))
			(:video-resize-event (:w w :h h)
					     (init-gl w h)
					     (process-event))
			(:video-expose-event () (process-event))
			(:sys-wm-event () (process-event))
			(:user-event () (process-event))
			(:idle ()
			       (handle-swank-requests)
			       (render-everything ui agar-module)
			       (gl:flush)
			       (sdl:update-display)
			       (with-slots (last-selection) ui
				 ;; invalidate last selection
				 (setf last-selection nil))
			       (sleep 0.1))))
		    (continuable-main-loop ()
		      (restart-case (main-loop)
			(continue-main-loop () (continuable-main-loop))
			(terminate-main-loop () (format t "~%main loop has been terminated")))))
	     (continuable-main-loop))))
    (setf (slot-value ui 'running-p) nil)))

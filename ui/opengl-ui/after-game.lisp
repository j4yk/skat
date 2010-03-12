(in-package gl-ui)

(defun declaration-atom-text (atom)
  (ecase atom
    (:with "Mit") (:without "Ohne") (1 "1") (2 "2") (3 "3") (4 "4")
    (:diamonds "Karo") (:hearts "Herz") (:spades "Pik") (:clubs "Kreuz")
    (:grand "Grand") (:hand "Hand") (:ouvert "Ouvert")
    (:declared-schneider "Schneider angesagt") (:played-schneider "Schneider gespielt")
    (:declared-schwarz "Schwarz angesagt") (:played-schwarz "Schwarz gespielt")))

(defun find-if-in-set (set sequence)
  (find-if (rcurry #'member set) sequence))

(defun first-declaration-part (declaration)
  (delete nil 
	  (mapcar (rcurry #'find-if-in-set declaration)
		  (list '(:diamods :hearts :spades :clubs :grand :null) '(:hand)
			'(:with :without) '(1 2 3 4)))
	  :count 4))

(defun schneider-declaration-part (declaration)
  (delete nil
	  (mapcar (rcurry #'find-if-in-set declaration)
		  (list '(:declared-schneider) '(:played-schneider)))
	  :count 2))

(defun schwarz-declaration-part (declaration)
  (delete nil
	  (mapcar (rcurry #'find-if-in-set declaration)
		  (list '(:declared-schwarz) '(:played-schwarz)))
	  :count 2))

(defun declaration-text (declarer declaration won-p)
  (format nil "~a spielte~%~a~%~a~%~a~%~a~%und ~a"
	  declarer
	  (format nil "~{~a ~}" (mapcar #'declaration-atom-text (first-declaration-part declaration)))
	  (format nil "~{~a ~}" (mapcar #'declaration-atom-text (schneider-declaration-part declaration)))
	  (format nil "~{~a ~}" (mapcar #'declaration-atom-text (schwarz-declaration-part declaration)))
	  (if (find :ouvert declaration) (declaration-atom-text :ouvert) "")
	  (if won-p "gewann" "verlor")))

(defclass game-report-window (agar-window)
  ((declarer :initarg :declarer)
   (defenders :initarg :defenders)
   (player-names :initarg :player-names)
   (DECLARER-NAME-FV) (WINDOW) (CARD-POINTS-HBOX) (HBOX-LVBOX) (HBOX-RVBOX)
 (DECLARER-NAME-LABEL) (DECLARER-CARD-POINTS-LABEL) (DEFENDERS-NAMES-LABEL)
 (DEFENDERS-CARD-POINTS-LABEL) (DECLARATION-PRELUDE-LABEL) (DECLARATION-LABEL)
 (SCORE-HBOX) (SCORE-HBOX-LVBOX) (PLAYER1-LABEL) (PLAYER2-LABEL)
 (PLAYER3-LABEL) (SCORE-HBOX-POINTSBOX) (PLAYER1-SCORE-LABEL)
 (PLAYER2-SCORE-LABEL) (PLAYER3-SCORE-LABEL) (SCORE-HBOX-DIFFBOX)
 (PLAYER1-DIFF-LABEL) (PLAYER2-DIFF-LABEL) (PLAYER3-DIFF-LABEL) (BUTTONS-HBOX)
 (NEXT-GAME-BTN) (LEAVE-BTN)))

(defmethod initialize-instance :after ((w game-report-window)
				       &key declarer defenders player-names
				       &allow-other-keys)
  (check-type defenders list)
  (let ((defenders-names (apply #'format nil "~a und ~a" defenders)))
    (let*-slots w
	((declarer-name-fv (make-foreign-variable :ptr (foreign-string-alloc declarer)
						  :size (foreign-string-size declarer)))
	 (window (ag:window-new))
	 (card-points-hbox (expanded-h (ag:hbox-new window)))
	 (hbox-lvbox (ag:vbox-new card-points-hbox))
	 (hbox-rvbox (ag:vbox-new card-points-hbox))
	 (declarer-name-label (ag:new-label hbox-lvbox nil "%s" (foreign-variable-ptr declarer-name-fv)))
	 (declarer-card-points-label (ag:new-label hbox-rvbox nil "..."))
	 (defenders-names-label (ag:new-label hbox-lvbox nil defenders-names))
	 (defenders-card-points-label (ag:new-label hbox-rvbox nil "..."))
	 (declaration-prelude-label (ag:new-label window nil "%s spielte" (foreign-variable-ptr declarer-name-fv)))
	 (declaration-label (ag:new-label window nil (format nil "...~%...~%...~%...~%...")))
	 (score-hbox (ag:hbox-new window))
	 (score-hbox-lvbox (ag:vbox-new score-hbox))
	 (player1-label (ag:new-label score-hbox-lvbox nil (first player-names)))
	 (player2-label (ag:new-label score-hbox-lvbox nil (second player-names)))
	 (player3-label (ag:new-label score-hbox-lvbox nil (third player-names)))
	 (score-hbox-pointsbox (expanded-h (ag:vbox-new score-hbox)))
	 (player1-score-label (ag:new-label score-hbox-pointsbox nil "....."))
	 (player2-score-label (ag:new-label score-hbox-pointsbox nil "....."))
	 (player3-score-label (ag:new-label score-hbox-pointsbox nil "....."))
	 (score-hbox-diffbox (ag:vbox-new score-hbox))
	 (player1-diff-label (ag:new-label score-hbox-diffbox nil "......"))
	 (player2-diff-label (ag:new-label score-hbox-diffbox nil "......"))
	 (player3-diff-label (ag:new-label score-hbox-diffbox nil "......"))
	 (buttons-hbox (ag:hbox-new window))
	 (next-game-btn (ag:new-button buttons-hbox nil "Nächstes Spiel"
				       (std-event-handler (next-game w))))
	 (leave-btn (ag:new-button buttons-hbox nil "Runde verlassen"
				   (std-event-handler (leave w)))))
      (ag:window-set-caption window "Spielauswertung")
      (ag:set-box-label card-points-hbox "Augenpunkte")
      (ag:set-box-padding card-points-hbox 12)
      (ag:set-box-label score-hbox "Punkte")
      (ag:set-box-padding card-points-hbox 12))))

(defclass after-game (module)
  ())


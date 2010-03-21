(in-package gl-ui)

(defun declaration-atom-text (atom)
  (ecase atom
    (:with "mit") (:without "ohne") (1 "1") (2 "2") (3 "3") (4 "4")
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

(defun declaration-text (declaration)
  (format nil "~a~%~a~%~a~%~a"
	  (format nil "~{~a ~}" (mapcar #'declaration-atom-text (first-declaration-part declaration)))
	  (format nil "~{~a ~}" (mapcar #'declaration-atom-text (schneider-declaration-part declaration)))
	  (format nil "~{~a ~}" (mapcar #'declaration-atom-text (schwarz-declaration-part declaration)))
	  (if (find :ouvert declaration) (declaration-atom-text :ouvert) "")))

(defclass game-report-window (agar-window)
  ((declarer :initarg :declarer)
   (defenders :initarg :defenders)
   (prompt-p :initarg :prompt-p)
   (DECLARER-NAME-FV) (WINDOW) (CARD-POINTS-HBOX) (HBOX-LVBOX) (HBOX-RVBOX)
   (DECLARER-NAME-LABEL) (DECLARER-CARD-POINTS-LABEL) (DEFENDERS-NAMES-LABEL)
   (DEFENDERS-CARD-POINTS-LABEL) (DECLARATION-PRELUDE-LABEL) (DECLARATION-LABEL)
   (won-or-lost-label)
   (SCORE-HBOX) (SCORE-HBOX-LVBOX) (PLAYER1-LABEL) (PLAYER2-LABEL)
   (PLAYER3-LABEL) (SCORE-HBOX-POINTSBOX) (PLAYER1-SCORE-LABEL)
   (PLAYER2-SCORE-LABEL) (PLAYER3-SCORE-LABEL) (SCORE-HBOX-DIFFBOX)
   (PLAYER1-DIFF-LABEL) (PLAYER2-DIFF-LABEL) (PLAYER3-DIFF-LABEL) (BUTTONS-HBOX)
   (NEXT-GAME-BTN) (LEAVE-BTN)))

(defmethod next-game ((w game-report-window))
  (next-game (module w)))

(defmethod leave ((w game-report-window))
  (leave (module w)))

(defmethod initialize-instance :after ((w game-report-window)
				       &key declarer defenders prompt-p
				       &allow-other-keys)
  (check-type defenders list)
  (let ((defenders-names (apply #'format nil "~a und ~a" defenders))
	(player-names (cons declarer defenders)))
    (let*-slots w
	((declarer-name-fv (make-foreign-variable :ptr (foreign-string-alloc declarer)
						  :size (foreign-string-size declarer)))
	 (window (ag:window-new))
	 (card-points-hbox (expanded-h (ag:hbox-new window)))
	 (hbox-lvbox (ag:vbox-new card-points-hbox))
	 (hbox-rvbox (ag:vbox-new card-points-hbox))
	 (declarer-name-label (expanded-h (ag:new-label hbox-lvbox nil "%s" (foreign-variable-ptr declarer-name-fv))))
	 (declarer-card-points-label (expanded-h (ag:new-label hbox-rvbox nil "......")))
	 (defenders-names-label (expanded-h (ag:new-label hbox-lvbox nil defenders-names)))
	 (defenders-card-points-label (expanded-h (ag:new-label hbox-rvbox nil "......")))
	 (declaration-prelude-label (expanded-h (ag:new-label window nil "%s spielte" (foreign-variable-ptr declarer-name-fv))))
	 (declaration-label (expanded-h (ag:new-label window nil "...")))
	 (won-or-lost-label (expanded-h (ag:new-label window nil (format nil "und ..."))))
	 (score-hbox (expanded-h (ag:hbox-new window)))
	 (score-hbox-lvbox (expanded (ag:vbox-new score-hbox)))
	 ;; player1 is the declarer here
	 (player1-label (expanded-h (ag:new-label score-hbox-lvbox nil (first player-names))))
	 (player2-label (expanded-h (ag:new-label score-hbox-lvbox nil (second player-names))))
	 (player3-label (expanded-h (ag:new-label score-hbox-lvbox nil (third player-names))))
	 (score-hbox-pointsbox (expanded-v (ag:vbox-new score-hbox)))
	 (player1-score-label (expanded-h (ag:new-label score-hbox-pointsbox nil ".......")))
	 (player2-score-label (expanded-h (ag:new-label score-hbox-pointsbox nil ".......")))
	 (player3-score-label (expanded-h (ag:new-label score-hbox-pointsbox nil ".......")))
	 (score-hbox-diffbox (expanded-v (ag:vbox-new score-hbox)))
	 (player1-diff-label (expanded-h (ag:new-label score-hbox-diffbox nil "......")))
	 (buttons-hbox (expanded-h (ag:hbox-new window)))
	 (next-game-btn (ag:new-button buttons-hbox nil "Nächstes Spiel"
				       (std-event-handler (next-game w))))
	 (leave-btn (ag:new-button buttons-hbox nil "Runde verlassen"
				   (std-event-handler (leave w)))))
      (ag:window-set-caption window "Spielauswertung")
      (unless prompt-p
	(ag:disable-widget next-game-btn))
      (ag:set-box-label card-points-hbox "Augenpunkte")
      (ag:set-box-padding card-points-hbox 12)
      (ag:set-box-label score-hbox "Punkte")
      (ag:set-box-padding score-hbox 12))))

(defmethod cards-score ((w game-report-window) declarer-score defenders-score)
  (with-slots (declarer-card-points-label defenders-card-points-label window) w
    (ag:label-string declarer-card-points-label (format nil "~a" declarer-score))
    (ag:label-string defenders-card-points-label (format nil "~a" defenders-score))
    (ag:window-update window)))

(defmethod show-declaration ((w game-report-window) declaration won-p)
  (with-slots (declaration-label won-or-lost-label) w
    (ag:label-string declaration-label (declaration-text declaration))
    (ag:label-string won-or-lost-label (format nil "und ~a" (if won-p "gewann" "verlor"))))
  (autosize w))

(defmethod show-score-table ((w game-report-window) score1 score2 score3)
  (with-slots (player1-score-label player2-score-label player3-score-label) w
    (ag:label-string player1-score-label (format nil "~a" score1))
    (ag:label-string player2-score-label (format nil "~a" score2))
    (ag:label-string player3-score-label (format nil "~a" score3)))
  (autosize w))

(defmethod show-score-difference ((w game-report-window) score)
  (with-slots (player1-diff-label) w
    (let ((text (format nil "~:[+~;~]~a" (minusp score) score)))
      (ag:label-string player1-diff-label text)
      (ag:size-hint-label player1-diff-label 0 text)))
  (autosize w))

(defclass after-game (module)
  ((game-report-window)))

(defmethod game-report-shown-p ((module after-game))
  (slot-boundp module 'game-report-window))

(defmethod show-game-report ((module after-game) prompt-p declarer defenders)
  (with-slots (game-report-window) module
    (setf game-report-window (make-instance 'game-report-window
					    :prompt-p prompt-p
					    :defenders defenders
					    :declarer declarer
					    :module module))
    (show game-report-window)))

(defmethod game-over-again ((module after-game) prompt-p)
  "Updates the enabled/disabled state of the next-game button"
  (game-over-again (slot-value module 'game-report-window) prompt-p))

(defmacro pass-through-to-slot (methodname slot-name class-name &rest arg-names)
  `(defmethod ,methodname ((module ,class-name) ,@arg-names)
     (with-slots (,slot-name) module
       (,methodname ,slot-name ,@arg-names))))

(defmacro pass-through-to-game-report-window (methodname &rest arg-names)
  `(pass-through-to-slot ,methodname game-report-window after-game ,@arg-names))

(pass-through-to-game-report-window cards-score declarer-score defenders-score)
(pass-through-to-game-report-window show-declaration declaration won-p)
(pass-through-to-game-report-window show-score-difference score)
(pass-through-to-game-report-window show-score-table score1 score2 score3)

(defmethod next-game ((module after-game))
  (call-kernel-handler (ui module) 'game-start)
  (with-slots (game-report-window) module
    (get-rid-of-window (window game-report-window)))
  (slot-makunbound module 'game-report-window))

(defmethod leave ((module after-game))
  (with-slots (game-report-window) module
    (get-rid-of-window (window game-report-window)))
  (slot-makunbound module 'game-report-window)
  (leave (ui module)))


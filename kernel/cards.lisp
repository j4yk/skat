;;;; Konventionen für die englischen Begrifflichkeiten:
;;; Spielfarbe = suit
;;; Kartenrang = rank
;;; Trumpf = trump
;;; Reizwerte und Spielwerte = game points
;;; Augenpunkte = card points
;;; angesagtes Spiel = game
;;; vollständige Ansage inkl. Schneider etc. = declaration

(in-package :skat-kernel)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (card :conc-name) suit rank)

  (defparameter *card-suits* '((:diamonds :d #\d #\D) 
			       (:hearts :h #\h #\H)
			       (:spades :s #\s #\S) 
			       (:clubs :c #\c #\C))
    "Token-Liste der vier Spielfarben")

  (defparameter *card-ranks* '((:seven 7) 
			       (:eight 8) 
			       (:nine 9) 
			       (:queen :q) 
			       (:king :k) 
			       (:ten 10) 
			       (:ace :a) 
			       (:jack :j))
    "Token-Liste der Kartenränge")

  (defparameter *card-points* '((:seven . 0)
				(:eight . 0)
				(:nine . 0)
				(:jack . 2)
				(:queen . 3)
				(:king . 4)
				(:ten . 10)
				(:ace . 11))
    "Die Zuordnung der Augen zu den Kartenrängen")

  (defparameter *suit-order-jacks* '(:diamonds :hearts :spades :clubs)
    "Die Reihenfolge für die Wertigkeit der Farben der vier Buben")

  (defparameter *suit-order* *suit-order-jacks*
    "Die Reihenfolge der vier Farben fürs Sortieren")

  (defparameter *rank-order* '(:seven :eight :nine :queen :king :ten :ace)
    "Die Reihenfolge für die Wertigkeit der Kartenränge")

  (defparameter *rank-order-null* '(:seven :eight :nine :ten :jack :queen :king :ace)
    "Die eingereihte Reihenfolge für die Wertigkeit der Kartenränge für Nullspiele")

  (defun translate-token (token list-of-token-lists)
    "Wandelt eine der Möglichkeiten für Spielfarben und Kartenrang aus den Listen oben
in die jeweils erstgenannte um, d. h. in das ausgeschriebene keyword."
    (let ((result (find (if (symbolp token) (to-keyword token) token)
			list-of-token-lists :test #'member)))
      (and result (car result))))

  (defun to-suit (thing)
    (translate-token thing *card-suits*))

  (defun to-rank (thing)
    (translate-token thing *card-ranks*))

  (defun read-card (&optional (stream *standard-input*) char arg)
    "Liest eine Karte aus einem Stream ein."
    (declare (ignore char arg))
    (let ((first-component (read stream)))
      (if (to-suit first-component)
	  (make-card :suit (to-suit first-component) :rank (to-rank (read stream)))
	  (with-input-from-string (s (format nil "~s" first-component)) ; Farbe nicht ausgeschrieben
	    (make-card :suit (to-suit (read-char s)) ; Farbe am ersten Buchstaben ablesen
		       :rank (to-rank (if (listen s)
					  (read s) ; kurzschreibweise, zB "D7"
					  (read stream)))))))) ; auseinander geschrieben, zB "D seven" oder "D 7"

  (defmacro card (&rest args)
    (with-input-from-string (s (apply #'concatenate 'string (mapcar #'string args)))
      (read-card s nil nil)))

  (set-dispatch-macro-character #\# #\! #'read-card)
  (set-dispatch-macro-character #\# #\c #'read-card) ; keine komplexen Zahlen notwendig

  (defmethod print-object ((card card) stream)
    (format stream "#!~a~a" (second (assoc (suit card) *card-suits*)) (second (assoc (rank card) *card-ranks*))))

  (defmethod make-load-form ((card card) &optional environment)
    "Stellt eine Form für den Compiler zur Verfügung, um
identische Kartenobjekte zu erzeugen."
    (declare (ignore environment))
    `(make-card :suit ,(suit card) :rank ,(rank card))))

(defun all-cards ()
  "Gibt alle 32 Karten zurück."
  (loop for suit in (mapcar #'car *card-suits*)
     append (loop for rank in (mapcar #'car *card-ranks*)
	       collect (make-card :suit suit :rank rank))))

(defun same-suit-p (card1 card2 game)
  "Gibt zurück, ob zwei Karten von der selben logischen Farbe sind.
Trümpfe gelten als eigene Farbe."
  (if (or (eq (suit card1) game)
	  (and (not (eq game :null)) (eq (rank card1) :jack)))
      ;; card1 is trump suit
      (or (eq (suit card2) game) (eq (rank card2) :jack))
      ;; no trump suit
      (and (eq (suit card1) (suit card2))
	   ;; jack would be trump unless it's a null game
	   (or (eq game :null) (not (eq (rank card2) :jack))))))

(deftest "same-suit-p für Bube und Trumpfkarte" :category "Cards"
	 :input-form (values #!DA #!HJ :diamonds)
	 :test-fn #'same-suit-p
	 :output-form t)

(deftest "same-suit-p für Bube und Nichttrumpfkarte" :category "Cards"
	 :input-form (values #!DA #!HJ :spades)
	 :test-fn #'same-suit-p
	 :output-form nil)

(deftest "same-suit-p für zwei Karten verschiedener Farben" :category "Cards"
	 :input-form (values #!DA #!HA :spades)
	 :test-fn #'same-suit-p
	 :output-form nil)

(deftest "same-suit-p für zwei Karten gleicher Farbe" :category "Cards"
	 :input-form (values #!D10 #!DA :hearts)
	 :test-fn #'same-suit-p
	 :output-form t)

(deftest "same-suit-p für zwei Trumpfkarten" :category "Cards"
	 :input-form (values #!D10 #!DA :diamonds)
	 :test-fn #'same-suit-p
	 :output-form t)

(defmethod compare-cards ((card1 card) (card2 card) (game (eql :null)))
  "Berechnet einen Vergleichswert der beiden Karten für ein Nullspiel.
Ist der Wert positiv ist die erste Karte höher als die zweite."
  (labels ((position-in-rank-order (card)
	     (position (rank card) *rank-order-null*)))
    (if (eq (suit card1) (suit card2))
	(reduce #'- (list card1 card2) :key #'position-in-rank-order)
	1)))

(defun jackp (card)
  "Gibt t zurück, wenn card ein Bube ist."
  (eq (rank card) :jack))

(defmethod compare-cards ((card1 card) (card2 card) (game symbol))
  "Berechnet einen Vergleichswert der beiden Karten für ein Farbenspiel.
Ist der Wert positiv ist die erste Karte höher als die zweite."
  (symbol-macrolet ((card1-wins 1)
		    (card2-wins -1))
    (if (same-suit-p card1 card2 game)
	(if (jackp card1)
	    (if (jackp card2)
		;; beide Karten sind Buben
		(reduce #'- (list card1 card2) ; Unterschied der Buben ausrechnen
			:key (lambda (c) (position (suit c) *suit-order-jacks*)))
		;; beide Karten sind Trumpf, nur die erste ist ein Bube
		card1-wins)    ; Buben sind immer die höchsten Trümpfe
	    (if (jackp card2)
		;; beide Karten sind Trumpf, jedoch nur die zweite ein Bube
		card2-wins     ; Buben sind immer die höchsten Trümpfe
		;; keine der beiden Karten ist ein Bube, aber gleichfarbig
		(reduce #'- (list card1 card2) ; Unterschied des Kartenranges ausrechnen
			:key (lambda (c) (position (rank c) *rank-order*)))))
	;; die Karten sind von unterschiedlicher Spielfarbe
	(if (or (jackp card1) (eq (suit card1) game))
	    card1-wins		; Karte 1 ist Trumpf
	    (if (or (jackp card2) (eq (suit card2) game))
		card2-wins		; Karte 2 ist Trumpf
		card1-wins)))))		; Karte 2 ist kein Trumpf, d. h. Karte 1 nimmt mit

(deftest "compare-cards mit zwei Buben im Farbspiel" :category "Cards"
	 :input-form (values #!HJ #!DJ :diamonds)
	 :test-fn #'compare-cards
	 :output-form 1)

(deftest "compare-cards mit zwei Buben im Grandspiel" :category "Cards"
	 :input-form (values #!HJ #!DJ :grand)
	 :test-fn #'compare-cards
	 :output-form 1)

(deftest "compare-cards mit zwei Buben im Nullspiel" :category "Cards"
	 :input-form (values #!DJ #!HJ :null)
	 :test-fn #'compare-cards
	 :output-form 1)

(deftests "compare-cards"
  ("Bube nimmt Farbe mit" (compare-cards #!HJ #!DA :grand) 1)
  ("Farbe wird von Bube mitgenommen" (compare-cards #!DA #!HJ :grand) -1)
  ("Bube nimmt Farbtrumpf mit" (compare-cards #!HJ #!DA :diamonds) 1)
  ("Farbtrumpf wird von Bube mitgenommen" (compare-cards #!DA #!HJ :diamonds) -1)
  ("Farbtrumpf nimmt Farbe mit" (compare-cards #!D7 #!HA :diamonds) 1)
  ("Farbtrumpf sticht Farbe" (compare-cards #!HA #!D7 :diamonds) -1)
  ("Farbe nimmt andere Farbe mit" (compare-cards #!D7 #!HA :grand) 1))

(defun card-greater-p (card1 card2 game)
  "Gibt zurück, ob card1 höher ist als card2"
  (plusp (compare-cards card1 card2 game)))

(defun greatest-card (game &rest cards)
  "Gibt die höchste der Karten zurück (Reihenfolge der Karten gilt!)"
  (if (null cards)
      nil
      (if (null (cdr cards))
	  (car cards)
	  (if (card-greater-p (car cards) (cadr cards) game)
	      (apply #'greatest-card game (cons (car cards) (cddr cards)))
	      (apply #'greatest-card game (cdr cards))))))

(defun sort-cards (cards game)
  "Sortiert die Karten für ein bestimmtes Spiel"
  (labels ((position-in-suit-order (card)
	     (position (suit card) *suit-order*))
	   (lower-p (card1 card2)
	     "Gibt t zurück, wenn card1 vor card2 einsortiert werden soll"
	     (if (same-suit-p card1 card2 game)
		 (not (card-greater-p card1 card2 game)) ; gleiche Farbe => Rang entscheidet
		 (symbol-macrolet ((decide-by-suit	 ; entscheide nach Farbenrangfolge
				    (< (reduce #'- (list card1 card2) :key #'position-in-suit-order) 0)))
		   (if (eq game :null)
		       decide-by-suit	; bei Nullspielen zählt nur die Farbe
		       (and (not (jackp card1)) ; bei anderen müssen die Buben höher sein
			    (or (jackp card2) decide-by-suit)))))))
    (sort cards #'lower-p)))

(defun card-points (card)
  "Gibt die Augenpunkte zurück, die eine Karte zählt."
  (cdr (assoc (rank card) *card-points*)))

(deftests "Karten"
  ("Punkte für Luschen" ((lambda (&rest cards) (values-list (mapcar #'card-points cards)))
			 #cD7 #cD8 #cD9)
			(values 0 0 0))
  ("Punkte für Bilder" ((lambda (&rest cards) (values-list (mapcar #'card-points cards)))
			#cDJ #cDQ #cDK)
		       (values 2 3 4))
  ("Punkte für Ass und 10" ((lambda (&rest cards) (values-list (mapcar #'card-points cards)))
			    #cDA #cD10)
			   (values 11 10)))

(defun jacks-flush-run (suits)
  "Gibt eine Liste aus :WITH oder :WITHOUT und der Anzahl der Spitzen zurück.
(Bspw. (list :WITHOUT 2))"
  (if (member :clubs suits)
      (values (list :with
		    (1+ (loop for suit in '(:spades :hearts :diamonds)
			   if (member suit suits)
			   sum 1 into flush-run
			   else return flush-run
			   finally (return flush-run)))))
      (values (list :without
		    (1+ (loop for suit in '(:spades :hearts :diamonds)
			   unless (member suit suits)
			   sum 1 into flush-run
			   else return flush-run
			   finally (return flush-run)))))))

(deftests "Karten"
  ("Spitzen Kreuz Pik Herz Karo" (jacks-flush-run '(:clubs :spades :hearts :diamonds)) '(:with 4))
  ("Spitzen Kreuz Pik Herz" (jacks-flush-run '(:clubs :spades :hearts)) '(:with 3))
  ("Spitzen Kreuz Pik" (jacks-flush-run '(:clubs :spades)) '(:with 2))
  ("Spitzen Kreuz" (jacks-flush-run '(:clubs)) '(:with 1))
  ("Spitzen Kreuz Herz" (jacks-flush-run '(:clubs :hearts)) '(:with 1))
  ("Spitzen Pik Herz Karo" (jacks-flush-run '(:spades :hearts :diamonds)) '(:without 1))
  ("Spitzen Herz Karo" (jacks-flush-run '(:hearts :diamonds)) '(:without 2))
  ("Spitzen Karo" (jacks-flush-run '(:diamonds)) '(:without 3))
  ("keine Spitzen" (jacks-flush-run '()) '(:without 4))
  ("Spitzen Pik Karo" (jacks-flush-run '(:spades :diamonds)) '(:without 1)))
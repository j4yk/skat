(in-package :skat-kernel)

(defstruct (card :conc-name (:print-object print-card)) suit value)

(defparameter *card-suits* '((:diamonds #\d #\D :d) 
			     (:hearts #\h #\H :h)
			     (:spades #\s #\S :s) 
			     (:clubs #\c #\C :c)))
(defparameter *card-values* '((:seven 7) 
			      (:eight 8) 
			      (:nine 9) 
			      (:queen :q) 
			      (:king :k) 
			      (:ten 10) 
			      (:ace :a) 
			      (:jack :j)))

(defparameter *suit-order-jacks* '(:diamonds :hearts :spades :clubs)
  "Die Wertigkeit der Farben der vier Buben")

(defparameter *values-order-null* '(:seven :eight :nine :ten :jack :queen :king :ace)
  "Die Wertigkeit der Kartenwerte bei einem Nullspiel")

(defparameter *values-order-normal* '(:seven :eight :nine :queen :king :ten :ace)
  "Die Wertigkeit der Kartenwerte bei einem Nullspiel")

(defun translate-token (token list-of-token-lists)
  (let ((result (find (if (symbolp token) (to-keyword token) token) list-of-token-lists :test #'member)))
    (and result (car result))))

(defun to-suit (thing)
  (translate-token thing *card-suits*))

(defun to-value (thing)
  (translate-token thing *card-values*))

(defun read-card (&optional (stream *standard-input*) first-char second-char)
  "Liest eine Karte aus einem Stream ein."
  (declare (ignore first-char second-char))
  (let ((first-component (read stream)))
    (if (to-suit first-component)
	(make-card :suit (to-suit first-component) :value (to-value (read stream)))
	(with-input-from-string (s (format nil "~s" first-component)) ; Farbe nicht ausgeschrieben
	  (make-card :suit (to-suit (read-char s)) ; Farbe am ersten Buchstaben ablesen
		     :value (to-value (if (listen s)
					  (read s) ; kurzschreibweise, zB "D7"
					  (read stream)))))))) ; auseinander geschrieben, zB "D seven" oder "D 7"

(set-dispatch-macro-character #\# #\c #'read-card)

(defmethod print-card (card stream)
  (format stream "#c ~a ~a" (symbol-name (suit card)) (symbol-name (value card))))

(defmethod make-load-form ((card card) &optional environment)
  "Stellt eine Form für den Compiler zur Verfügung, um
identische Kartenobjekte zu erzeugen."
  (declare (ignore environment))
  `(make-card :suit ,(suit card) :value ,(value card)))

(defun all-cards ()
  "Gibt alle 32 Karten zurück."
  (loop for suit in (mapcar #'car *card-suits*)
     append (loop for value in (mapcar #'car *card-values*)
	       collect (make-card :suit suit :value value))))

(defun same-suit-p (card1 card2 game-variant)
  "Gibt zurück, ob zwei Karten von der selben logischen Farbe sind.
Trümpfe gelten als eigene Farbe."
  (if (or (eq (suit card1) game-variant)
	  (and (not (eq game-variant :null)) (eq (value card1) :jack)))
      (or (eq (suit card2) game-variant) (eq (value card2) :jack))
      (eq (suit card1) (suit card2))))

(deftest "same-suit-p für Bube und Trumpfkarte" :category "Cards"
	 :input-form (values #cDA #cHJ :diamonds)
	 :test-fn #'same-suit-p
	 :output-form t)

(deftest "same-suit-p für Bube und Nichttrumpfkarte" :category "Cards"
	 :input-form (values #cDA #cHJ :spades)
	 :test-fn #'same-suit-p
	 :output-form nil)

(deftest "same-suit-p für zwei Karten verschiedener Farben" :category "Cards"
	 :input-form (values #cDA #cHA :spades)
	 :test-fn #'same-suit-p
	 :output-form nil)

(deftest "same-suit-p für zwei Karten gleicher Farbe" :category "Cards"
	 :input-form (values #cD10 #cDA :hearts)
	 :test-fn #'same-suit-p
	 :output-form t)

(deftest "same-suit-p für zwei Trumpfkarten" :category "Cards"
	 :input-form (values #cD10 #cDA :diamonds)
	 :test-fn #'same-suit-p
	 :output-form t)

(defmethod compare-cards ((card1 card) (card2 card) (game-variant (eql :null)))
  "Berechnet einen Vergleichswert der beiden Karten für ein Nullspiel.
Ist der Wert positiv ist die erste Karte höher als die zweite."
  (labels ((position-in-values-order (card)
	     (position (value card) *values-order-null*)))
    (if (eq (suit card1) (suit card2))
	(reduce #'- (list card1 card2) :key #'position-in-values-order)
	1)))

(defun jack-p (card)
  "Gibt t zurück, wenn card ein Bube ist."
  (eq (value card) :jack))

(defmethod compare-cards ((card1 card) (card2 card) (game-variant symbol))
  "Berechnet einen Vergleichswert der beiden Karten für ein Farbenspiel.
Ist der Wert positiv ist die erste Karte höher als die zweite."
  (symbol-macrolet ((card1-wins 1)
		    (card2-wins -1))
    (if (same-suit-p card1 card2 game-variant)
	(if (jack-p card1)
	    (if (jack-p card2)
		;; beide Karten sind Buben
		(reduce #'- (list card1 card2) ; Unterschied der Buben ausrechnen
			:key (lambda (c) (position (suit c) *suit-order-jacks*)))
		;; beide Karten sind Trumpf, nur die erste ist ein Bube
		card1-wins)    ; Buben sind immer die höchsten Trümpfe
	    (if (jack-p card2)
		;; beide Karten sind Trumpf, jedoch nur die zweite ein Bube
		card2-wins     ; Buben sind immer die höchsten Trümpfe
		;; keine der beiden Karten ist ein Bube, aber gleichfarbig
		(reduce #'- (list card1 card2) ; Unterschied des Kartenwertes ausrechnen
			:key (lambda (c) (position (value c) *values-order-normal*)))))
	;; die Karten sind von unterschiedlicher Spielfarbe
	card1-wins)))

(deftest "compare-cards mit zwei Buben im Farbspiel" :category "Cards"
	 :input-form (values #c hearts jack #c diamonds jack :diamonds)
	 :test-fn #'compare-cards
	 :output-form 1)

(deftest "compare-cards mit zwei Buben im Grandspiel" :category "Cards"
	 :input-form (values #c hearts jack #c diamonds jack :grand)
	 :test-fn #'compare-cards
	 :output-form 1)

;; Vorschlag für Makro:
;; (deftests
;;     ("text" (compare-cards #c diamonds jack #c hearts jack :null) 1)
;;     ("text" (function-name input-form* ) output-form))

(deftest "compare-cards mit zwei Buben im Nullspiel" :category "Cards"
	 :input-form (values #c diamonds jack #c hearts jack :null)
	 :test-fn #'compare-cards
	 :output-form 1)

(defun card-greater-p (card1 card2 game-variant)
  "Gibt zurück, ob card1 höher ist als card2"
  (plusp (compare-cards card1 card2 game-variant)))

(defun greatest-card (game-variant &rest cards)
  "Gibt die höchste der Karten zurück (Reihenfolge der Karten gilt!)"
  (if (null cards)
      nil
      (if (null (cdr cards))
	  (car cards)
	  (if (card-greater-p (car cards) (cadr cards) game-variant)
	      (apply #'greatest-card game-variant (cons (car cards) (cddr cards)))
	      (apply #'greatest-card game-variant (cdr cards))))))

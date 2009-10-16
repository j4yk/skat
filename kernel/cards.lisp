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

(defmethod compare-cards ((card1 card) (card2 card) (game-variant (eql :null)))
  "Berechnet einen Vergleichswert der beiden Karten für ein Nullspiel."
  (labels ((position-in-values-order (card)
	     (position (value card) *values-order-null*)))
    (if (eq (suit card1) (suit card2))
	(reduce #'- (list card1 card2) :key #'position-in-values-order)
	1)))

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

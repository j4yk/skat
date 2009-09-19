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

(defun to-keyword (symbol)
  "Gibt ein Symbol gleichen Namens aus dem Package keyword zurück"
  (intern (symbol-name symbol) :keyword))

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
  (format stream "#c ~a ~a" (symbol-name (card-suit card)) (symbol-name (card-value card))))

(defun all-cards ()
  (loop for suit in *card-suits*
     append (loop for value in *card-values*
	       collect (make-card :suit suit :value value))))
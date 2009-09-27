(defpackage :cards-test
  (:use :cl :skat-kernel :xlunit)
  (:export #:cards-test))

(in-package :cards-test)

(defclass cards-test (xlunit:test-case)
  ())

(def-test-method to-suit-test ((test cards-test))
  (loop for possibilities in kern::*card-suits*
     do (loop for p in possibilities
	   do (xlunit:assert-equal (kern::to-suit p) (car possibilities)))))

(xlunit:def-test-method to-value-test ((test cards-test))
  (loop for possibilities in kern::*card-values*
     do (loop for p in possibilities
	   do (xlunit:assert-equal (kern::to-value p) (car possibilities)))))

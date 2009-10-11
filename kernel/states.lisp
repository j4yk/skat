(in-package skat-kernel)

(defun state-entering-function (state)
  "Gibt den Namen einer Zustandseingangsfunktion
für einen bestimmten Zustand zurück."
  (declare (type symbol state))
  (intern (concatenate 'string "ENTER-" (symbol-name state) "-STATE") 'skat-kernel))

(defmacro define-state-entering-function (state kernel-class &body body)
  "Definiert eine neue Zustandseingangsfunktion."
  `(defmethod ,(state-entering-function state) ((,kernel-class ,kernel-class))
     ,@body))
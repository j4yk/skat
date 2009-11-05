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

(defmacro define-state-switch-function (state (kernel-class &rest args) &body body)
  "Definiert eine Methode, die body ausführt und dann den Kernel in den angegebenen
Zustand überführt, und ein Makro, welches diese Funktion aus einer Kernelmethode
oder einem Request-Handler (defhandler) heraus aufrufen kann."
  (let ((methodname (intern (concatenate 'string "SWITCH-TO-" (symbol-name state)))))
    `(prog1
	 ;; die eigentliche Methode:
	 (defkernelmethod ,methodname (,kernel-class ,@args)
	   ,@body
	   (switch-state kernel ',state))
       ;; und das Makro:
       (defmacro ,state ,args
	 `(,',methodname kernel ,,@args)))))

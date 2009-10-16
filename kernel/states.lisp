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
(defkernelmethod) oder einem Request-Handler (defhandler) heraus aufrufen kann.
(define-state-switch-function registration (player <argumente>)
  ...)
==> Methode switch-to-registration ((player player) <argumente>),
    Makro registration
Benutzung in defhandler oder defkernelmethod: (registration <argumente>)"
  (let ((methodname (intern (concatenate 'string "SWITCH-TO-" (symbol-name state)))))
    ;; Argumente auftrennen in normale, key-Argumente und Restargumente
    (multiple-value-bind (normal-args key-args rest-args) (separate-lambda-list args)
      `(prog1 
	   (defkernelmethod ,methodname (,kernel-class ,@args)
	     ,@(append body `((switch-state kernel ',state)))) ; die eigentliche Methode
	 ;; jetzt noch das Makro
	 (defmacro ,state (,@args)
	   `(apply ,(function ,methodname) kernel ,,@normal-args ; normale Argumente
		   ,,@(loop for a in key-args ; key-Argumente für den Aufruf umformen
			append `(,(to-keyword a) ,a))
		   ,,(car rest-args))))))) ; und den Rest fürs apply nehmen

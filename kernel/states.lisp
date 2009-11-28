(in-package skat-kernel)

(defun state-entering-function (state)
  "Gibt den Namen einer Zustandseingangsfunktion
für einen bestimmten Zustand zurück."
  (declare (type symbol state))
  (warn "state-entering-function ist veraltet.")
  (intern (concatenate 'string "ENTER-" (symbol-name state) "-STATE") 'skat-kernel))

(defmacro define-state-entering-function (state kernel-class &body body)
  "Definiert eine neue Zustandseingangsfunktion."
  (warn "define-state-entering-function ist veraltet.")
  `(defmethod ,(state-entering-function state) ((,kernel-class ,kernel-class))
     ,@body))

(defmacro define-state-switch-function (state (kernel-class &rest args) &body body)
  "Definiert eine Methode, die body ausführt und dann den Kernel in den angegebenen
Zustand überführt, und ein Makro, welches diese Funktion aus einer Kernelmethode
oder einem Request-Handler (defhandler) heraus aufrufen kann."
  (eval-when (:compile-toplevel)
    (if (member state (cdr (assoc kernel-class *kernel-states*)))
	(format t "~%State-Switch-Funktion ~a für ~a ist gültig." state kernel-class)
	(error "~a ist kein definierter Zustand für ~a" state kernel-class)))
  (let ((methodname (intern (concatenate 'string "SWITCH-TO-" (symbol-name state)))))
    `(progn
       ;; das Switch-Makro:
;;        (defmacro ,state ,args
;; 	 `(,',methodname kernel ,,@args))
       ;; die eigentliche Methode:
       (defkernelmethod ,methodname (,kernel-class ,@args)
	 ,@body
	 (switch-state kernel ',state)))))

(defmacro case-state (kernel-classname &body cases)
  "Erstellt ein Case-Statement über (state player) und prüft aber,
ob alle States gültig sind für diesen Kerneltyp.
Voraussetzungen:
- Variable Kernel muss mit dem Kernelobjekt gebunden sein."
  (declare (ignorable kernel-classname))
  (eval-when (:compile-toplevel)
    (dolist (state (mapcar #'car cases))
      (format t "~%checking ~a as state for ~a... state-is-valid-p == " state kernel-classname)
      (unless (princ (state-is-valid-p state kernel-classname))
	(error "~a ist kein definierter Zustand für ~a" state kernel-classname))
      (terpri)))
  `(ecase (state kernel)
     ,@cases))

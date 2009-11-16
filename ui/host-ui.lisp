(in-package skat-ui)

(defclass host-ui (base-ui)
  ((stop-received :documentation "t, nachdem die Methode stop aufgerufen wurde")
   (standard-output :documentation "Standard Output stream" :initarg :stdout)
   (debug-stream :accessor debug-stream :initarg :debugstream))
  (:documentation "Unsichtbare UI, ist nur als logische Komponente eine Schnittstelle."))

;;; host-ui implementiert keine Handler, da bestenfalls auch keine Anfragen an eine
;;; Benutzerschnittstellenschicht weitergeleitet werden sollten.
;;; Jedoch muss in dieser Komponente hier die Hauptschleife laufen.

(defmethod main-loop ((ui host-ui))
  "Die Hauptschleife für den Host.
Endet, wenn Slot stop-received initialisiert wird."
  (loop
     (sleep 0.5)
     (kernel:receive-requests (kernel ui))
     (when (slot-boundp ui 'stop-received)
       (return))))

(defun error-in-host-main-loop (condition ui)
  "Gibt den Fehler aus und lässt die Schleife weiterlaufen (continue restart)"
;  (format (debug-stream ui) "~a: ~a" (type-of condition) condition)
  (continue condition))

(defmethod start-main-loop ((ui host-ui))
  "Startet die Hauptschleife für den Host."
;;   (when (slot-boundp ui 'standard-output)
;;     (setq *standard-output* (slot-value ui 'standard-output))
;;     (format *standard-output* "*standard-output* im UI-Loop gebunden")
;;     (format t "test")) ; für SLIME
;;   (when (slot-boundp ui 'debug-stream)
;;     (setq *debug-io* (slot-value ui 'debug-stream))
;;     (format *debug-io* "*debug-io* im UI-Loop gebunden"))
  (handler-bind ((error #'(lambda (&optional condition) (error-in-host-main-loop condition ui))))
    (restart-case (main-loop ui)
      (continue (condition)
	(declare (ignorable condition)) (main-loop ui)))))

(defmethod start ((ui host-ui) &optional no-new-thread-p)
  "Startet die Hauptschleife für den Host in einem neuen Thread."
  (slot-makunbound ui 'stop-received)
  (if no-new-thread-p
      (main-loop ui)
      (sb-thread:make-thread #'(lambda () (main-loop ui)) :name "Host Main Loop Thread")))

(defmethod stop ((ui host-ui))
  "Hält die UI an, indem die Abbruchvariable gesetzt wird."
  (setf (slot-value ui 'stop-received) t))
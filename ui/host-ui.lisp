(in-package skat-ui)

(defclass host-ui (base-ui)
  ((stop-received :documentation "t, nachdem die Methode stop aufgerufen wurde")
   (standard-output :documentation "Standard Output stream" :initarg :stdout)
   (debug-stream :accessor debug-stream :initarg :debugstream))
  (:documentation "Unsichtbare UI, ist nur als logische Komponente eine Schnittstelle."))

;;; host-ui implementiert keine Handler, da bestenfalls auch keine Anfragen an eine
;;; Benutzerschnittstellenschicht weitergeleitet werden sollten.
;;; Jedoch muss in dieser Komponente hier die Hauptschleife laufen.

(defmethod main-loop-once ((ui host-ui))
  (restart-case (kernel:receive-requests (kernel ui))
    (continue-main-loop ()
      :report "setze die Hauptschleife der Host-UI fort"
      (values))
    (quit-main-loop ()
      :report "Hauptschleife der Host-UI beenden"
      (setf (slot-value ui 'stop-received) t)))
  (when (slot-boundp ui 'stop-received)
    'STOP-RECEIVED))

(defmethod main-loop ((ui host-ui))
  "Führt die Hauptschleife für den Host aus.
Endet, wenn Slot stop-received initialisiert wird."
  (loop
     (sleep 0.5)
     (when (eq (main-loop-once ui) 'STOP-RECEIVED)
       (return))))

(defmethod just-one-step ((ui host-ui))
  (main-loop-once ui))

(defmethod main-loop-in-other-thread ((ui host-ui))
  "Startet die Hauptschleife für den Host. Soll aus einem anderen Thread heraus gestartet werden."
  (handler-bind ((error #'(lambda () (invoke-restart 'continue-main-loop))))
    (main-loop ui)))

(defmethod start ((ui host-ui) &optional new-thread-p)
  "Startet die Hauptschleife für den Host in einem neuen Thread."
  (slot-makunbound ui 'stop-received)
  (if new-thread-p
      (sb-thread:make-thread #'(lambda () (main-loop-in-other-thread ui)) :name "Host Main Loop Thread")
      (main-loop ui)))

(defmethod stop ((ui host-ui))
  "Hält die UI an, indem die Abbruchvariable gesetzt wird."
  (setf (slot-value ui 'stop-received) t))
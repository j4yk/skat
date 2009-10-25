(in-package skat-ui)

(defclass host-ui (base-ui)
  ()
  (:documentation "Unsichtbare UI, ist nur als logische Komponente eine Schnittstelle."))

;;; host-ui implementiert keine Handler, da bestenfalls auch keine Anfragen an eine
;;; Benutzerschnittstellenschicht weitergeleitet werden sollten.
;;; Jedoch muss in dieser Komponente hier die Hauptschleife laufen.

(defmethod main-loop ((ui host-ui))
  "Die Hauptschleife für den Host"
  (warn "Bis jetzt kein Stop implementiert!")
  (loop
       (kernel:receive-requests (kernel ui))))

(defmethod start ((ui host-ui) &optional no-new-thread-p)
  "Startet die Hauptschleife für den Host in einem neuen Thread."
  (labels ((start-main-loop ()
	     (main-loop ui)))
    (if no-new-thread-p
	(start-main-loop)
	(sb-thread:make-thread #'start-main-loop :name "Host Main Loop Thread"))))

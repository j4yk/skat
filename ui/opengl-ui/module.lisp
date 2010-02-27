(in-package skat-opengl-ui)

(defclass module ()
  ((ui :initarg :ui :accessor ui)
   (submodules :initform nil :accessor submodules :type list))
  (:documentation "Basisklasse für alle OpenGL-UI Module"))

;; Module sollten (initialize-instance :after) implementieren,
;; um Aufbauarbeiten (Agar: Fenster erstellen etc.) durchzuführen

(defgeneric cleanup (module)
  (:documentation "Lässt ein Modul von ihm reservierten Speicher freigeben, Fenster schließen etc."))

(defgeneric draw (module)
  (:documentation "Lässt ein Modul seine Grafiken zeichnen"))

(defgeneric handle-event (module event)
  (:documentation "Lässt ein Modul das SDL_Event verarbeiten"))

(defmethod cleanup ((module module))
  "Default: No-op.")

(defmethod draw ((module module))
  "No-op as fallback")

(defmethod handle-event ((module module) event)
  "No-op as fallback")

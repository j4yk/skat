(in-package skat-opengl-ui)

(defclass module ()
  ((ui :initarg :ui :accessor ui)
   (submodules :initform nil :accessor submodules :type list))
  (:documentation "Basisklasse für alle OpenGL-UI Module"))

;; Module sollten (initialize-instance :after) implementieren,
;; um Aufbauarbeiten (Agar: Fenster erstellen etc.) durchzuführen

(defgeneric draw (module)
  (:documentation "Lässt ein Modul seine Grafiken zeichnen"))

(defgeneric handle-event (module event)
  (:documentation "Lässt ein Modul das SDL_Event verarbeiten"))

(defmethod handle-event ((module module) event)
  "No-op as fallback")

(defmethod draw ((module module))
  "No-op as fallback")

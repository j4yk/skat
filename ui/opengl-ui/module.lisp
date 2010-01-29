(in-package skat-opengl-ui)

(defclass module ()
  ((submodules :initform nil :accessor submodules :type list))
  (:documentation "Basisklasse für alle OpenGL-UI Module"))

;; Module sollten (initialize-instance :after) implementieren,
;; um Aufbauarbeiten (Agar: Fenster erstellen etc.) durchzuführen

(defgeneric draw (module)
  (:documentation "Lässt ein Modul seine Grafiken zeichnen"))

(defgeneric handle-event (module event)
  (:documentation "Lässt ein Modul das SDL_Event verarbeiten"))

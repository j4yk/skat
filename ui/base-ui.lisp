(in-package skat-ui)

(defclass base-ui ()
  ((kernel :accessor kernel :initarg :kernel))
  (:documentation "Basisklasse für alle Ausprägungen von Benutzerschnittstellen und Hauptschleifenobjekten."))

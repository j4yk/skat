(in-package skat-ui)

(defclass base-ui ()
  ((kernel :accessor kernel :initarg :kernel))
  (:documentation "Basisklasse für alle Ausprägungen von Benutzerschnittstellen und Hauptschleifenobjekten."))

(defgeneric start (ui &optional no-new-thread-p)
  (:documentation "Startet die Benutzerschnittstelle.
Gebärt dies in einer Implementierung normalerweise einen neuen Thread, kann der UI
durch setzen von no-new-thread-p auf t signalisiert werden, dass sie davon absehen soll."))

(defgeneric stop (ui)
  (:documentation "Signalisiert einer UI, dass sie ihre Arbeit so bald wie möglich
einstellen soll."))

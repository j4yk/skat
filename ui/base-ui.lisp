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

(defgeneric just-one-step (ui)
  (:documentation "Wenn eine UI dies implementiert, weist man sie hiermit an, nur eine Hauptschleifenausführung vorzunehmen."))

(defun call-kernel-handler (ui request-name &rest request-args)
  "Calls the Kernel's appropriate handler function with UI as the sender of the request"
  (assert (kern:handler-fn request-name) () "Kernel has no handler defined for request ~s" request-name)
  (if (slot-boundp ui 'kernel)
      (apply (kern:handler-fn request-name) (kernel ui) ui request-args)
      (warn "No kernel that can receive ~s ~s" request-name request-args)))

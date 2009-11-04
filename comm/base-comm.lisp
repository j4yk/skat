(in-package skat-comm)

(defstruct queue "Eine queue aus cons-Zellen. Zu verwenden mit queue-push und queue-pop." (elements nil) end)

(defun queue-push (obj queue)
  "Fügt ein neues Element in die Queue ein."
  (if (null (queue-elements queue))
      (setf (queue-elements queue) (cons obj nil)
	    (queue-end queue) (queue-elements queue))
      (setf (cdr (queue-end queue)) (cons obj nil)
	    (queue-end queue) (cdr (queue-end queue)))))

(defun queue-pop (queue)
  "Holt ein Element aus der Queue heraus."
  (pop (queue-elements queue)))

(defun queue-peek (queue)
  "Gibt das Element zurück, das als nächstes aus der Queue gepopt würde."
  (car (queue-elements queue)))

(defclass base-comm ()
  ((arrived-requests :accessor arrived-requests :initform (make-queue))
   (address-compare-function :reader address-compare-function :allocation :class))
  (:documentation "Fundamental Communication system. Implements no useable connection.
Meant as base class for other communication-classes."))

(defmethod push-request ((comm base-comm) sender request-name &rest request-args)
  "Fügt eine Anfrage zur Queue hinzu."
  (queue-push (append (list request-name sender) request-args) (arrived-requests comm))
  (cons request-name request-args))

(defgeneric start (comm)
  (:documentation "Initialisiert das Kommunikationsobjekt
und nimmt eventuelle Startaktionen inklusive Thread-Spawning vor."))

(defgeneric login (comm data)
  (:documentation "Weist das Kommunikationsobjekt an, sich mit den übergebenen
Daten in seinem Medium zu authentifizieren."))

(defgeneric send (comm address request-name &rest request-args)
  (:documentation "Weist das Kommunikationsobjekt an, die übergebene Anfrage
und ihre Parameter an die übergebene Adresse zu schicken."))

(define-condition received-other-content ()
  ((message :accessor message :initarg :message)))

(defgeneric receive-requests (comm)
  (:documentation "Weist das Kommunikationsobjekt an, alle eingetroffenen
Nachrichten abzuholen, sodass diese mit [[get-request]] geholt werden können"))

(defmethod get-request ((comm base-comm))
  "Gibt die erste Anfrage in der Warteschlange zurück.
Rückgabewerte: 1. Bezeichnung der Anfrage - ein Symbol
               2. Sender
               3. Argumentenliste - eine Liste"
  (receive-requests comm)		; Anfragen abrufen, wenn welche vorliegen
  (let ((top-request (queue-pop (arrived-requests comm))))
    (values (car top-request) (cadr top-request) (cddr top-request))))

(defmethod has-request ((comm base-comm))
  "Gibt t zurück, wenn sich Anfragen mit get-request abrufen ließen."
  (receive-requests comm)		; Anfragen abrufen, wenn welche vorliegen
  (not (null (queue-peek (arrived-requests comm)))))

(defgeneric stop (comm)
  (:documentation "Beendet das Kommunikationsobjekt
indem es zum Beispiel die Auswahl (Logout) aus dem Kommunikationsmedium vornimmt."))

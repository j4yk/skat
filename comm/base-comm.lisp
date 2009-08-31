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

(defun queue-next (queue)
  "Gibt das Element zurück, das als nächstes aus der Queue gepopt würde."
  (car (queue-elements queue)))

(defclass base-comm ()
  ((arrived-requests :accessor arrived-requests :initform (make-queue))))

(defun push-request (comm sender request-name &rest request-args)
  "Fügt eine Anfrage zur Queue hinzu."
  (queue-push (append (list request-name sender) request-args) (arrived-requests comm))
  (cons request-name request-args))

(defmethod get-request ((comm base-comm))
  "Gibt die erste Anfrage in der Warteschlange zurück.
Zwei Rückgabewerte: 1. Bezeichnung der Anfrage - ein Symbol
                    2. Sender
                    3. Argumentenliste - eine Liste"
  (let ((top-request (queue-pop (arrived-requests comm))))
    (values (car top-request) (cadr top-request) (cddr top-request))))

(defmethod has-request ((comm base-comm))
  "Gibt t zurück, wenn sich Anfragen mit get-request abrufen ließen."
  (not (null (queue-next (arrived-requests comm)))))

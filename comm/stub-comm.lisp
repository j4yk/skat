(in-package skat-comm)

(defclass stub-comm (base-comm)
  ((id :accessor id :initform (gensym "STUB-COMM"))
   (address-compare-function :initform #'eq)))

(defmethod host-address ((comm stub-comm) data)
  (getf data :host-comm))

(defun stub-msg (format-str &rest format-args)
  (apply #'format t format-str format-args)
  (terpri))

(defmethod print-object ((stub-comm stub-comm) stream)
  (print-unreadable-object (stub-comm stream :type t)
    (princ (id stub-comm) stream)))

(defmethod address ((stub-comm stub-comm))
  "Gibt sich selbst als Adresse zurück."
  stub-comm)

(defmethod start ((comm stub-comm))
  (stub-msg "COMM: started.")
  (push-request comm comm 'login-parameters (list nil))
  (values))

(defmethod login ((comm stub-comm) data)
  (stub-msg "COMM: logged in.")
  (push-request comm comm 'own-address (list (address comm)))
  (push-request comm comm 'registration-parameters (list '((host-comm stub-comm "Stub-Comm des Hosts"))))
  (values))

(defmethod register ((comm stub-comm) data)
  (print data)
  (push-request (getf data :host-comm) comm 'registration-request nil))

(defmethod push-request :after ((comm stub-comm) sender request-name request-args)
  "Schreibt auf, was gepusht wurde"
  (stub-msg "COMM: pushed ~a ~a from ~a" request-name request-args sender))

(defmethod get-request :around ((comm stub-comm))
  "Schreibt auf, was gepoppt wurde."
  (multiple-value-bind (request-name sender request-args) (call-next-method)
    (stub-msg "popped: ~a (~{~a ~}) from ~a" request-name request-args sender)
    (values request-name sender request-args)))

(define-condition stub-communication-send ()
  ((request-name :accessor request-name :initarg :request)
   (args :accessor args :initarg :args)
   (sender-comm :accessor sender-comm :initarg :sender)
   (address :accessor address :initarg :address))
  (:documentation "Condition zum automatisierten Weiterverarbeiten einer Sendung."))

(defmethod send ((comm stub-comm) address request-name &rest request-args)
  (stub-msg "COMM: sending ~a ~a to ~a" request-name request-args address)
  ;; lasse wen will hier einhängen, damit noch was automatisiert werden kann
  (restart-case (signal 'stub-communication-send :request request-name :args request-args
			:sender comm :address address)
    (continue () t))
  (values))

(defmethod stop ((comm stub-comm))
  (values))

(defmethod receive-requests ((comm stub-comm))
  "Tut eigentlich nichts."
  t)
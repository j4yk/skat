(in-package skat-comm)

(defclass stub-comm (base-comm)
  ((address-compare-function :initform #'eq)))

(defun stub-msg (format-str &rest format-args)
  (apply #'format t format-str format-args)
  (terpri))

(defmethod start ((comm stub-comm))
  (stub-msg "COMM: started.")
  (push-request comm comm 'login-parameters nil)
  (values))

(defmethod login ((comm stub-comm) data)
  (stub-msg "COMM: logged in.")
  (values))

(defmethod push-request :after ((comm stub-comm) sender request-name &rest request-args)
  "Schreibt auf, was gepusht wurde"
  (stub-msg "COMM: pushed ~a ~a from ~a" request-name request-args sender))

(define-condition stub-communication-send ()
  ((request-name :accessor request-name :initarg :request)
   (args :accessor args :initarg :args)
   (sender-comm :accessor sender-comm :initarg :sender)
   (address :accessor address :initarg :address))
  (:documentation "Condition zum automatisierten Weiterverarbeiten einer Sendung."))

(defmethod send ((comm stub-comm) address request-name &rest request-args)
  (stub-msg "COMM: sending ~a ~a to ~a" request-name request-args address)
  (signal 'stub-communication-send :request request-name :args request-args
	  :sender comm :address address)
  (values))

(defmethod stop ((comm stub-comm))
  (values))

(defmethod feed-comm ((comm stub-comm) sender request-name &rest request-args)
  "\"Füttert\" eine Stub-Comm mit einer Anfrage. So als ob sie sie empfangen hätte."
  (apply #'push-request comm sender request-name request-args))

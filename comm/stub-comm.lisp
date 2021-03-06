(in-package skat-comm)

(defclass stub-comm (base-comm)
  ((id :accessor id :initform (gensym "STUB-COMM") :initarg :id)
   (address-compare-function :initform #'eq)))

(defvar *stub-comm-verbosity* 2 "Gibt an, wie viele Nachrichten stub-comms drucken.
0 = keine, 1 = alles außer push-request, 2 = alles")

(defmacro verbose (min-level &body body)
  "Führt body nur aus, wenn *stub-comm-verbosity* mindestens min-level ist"
  `(when (>= *stub-comm-verbosity* ,min-level)
     ,@body))


(kern:define-login-data stub-login-data)
(kern:define-registration-data stub-registration-data (host-comm nil :type stub-comm))

(defmethod host-address ((comm stub-comm) (data stub-registration-data))
  (stub-registration-data-host-comm data))

(defun stub-comm-msg (comm format-str &rest format-args)
  (apply #'format t (concatenate 'string "~%~a: " format-str) comm format-args))

(defmethod print-object ((stub-comm stub-comm) stream)
  (print-unreadable-object (stub-comm stream :type t)
    (princ (id stub-comm) stream)))

(defmethod address ((stub-comm stub-comm))
  "Gibt sich selbst als Adresse zurück."
  stub-comm)

(defmethod start ((comm stub-comm))
  (verbose 1 (stub-comm-msg comm "started."))
  (push-request comm comm 'login-struct (list nil))
  (values))

(defmethod login ((comm stub-comm) data)
  (verbose 1 (stub-comm-msg comm "logged in."))
  (push-request comm comm 'own-address (list (address comm)))
  (push-request comm comm 'registration-struct '(stub-registration-data))
  (values))

(defmethod register ((comm stub-comm) (data stub-registration-data))
  (push-request (stub-registration-data-host-comm data) comm 'registration-request nil))

(defmethod push-request :after ((comm stub-comm) sender request-name request-args)
  "Schreibt auf, was gepusht wurde"
  (verbose 2 (stub-comm-msg comm "pushed ~s ~s from ~s" request-name request-args sender)))

(defmethod get-request :around ((comm stub-comm))
  "Schreibt auf, was gepoppt wurde."
  (multiple-value-bind (request-name sender request-args) (call-next-method)
    (verbose 1 (stub-comm-msg comm "popped ~s ~s from ~s" request-name request-args sender))
    (values request-name sender request-args)))

(define-condition stub-communication-send ()
  ((request-name :accessor request-name :initarg :request)
   (args :accessor args :initarg :args)
   (sender-comm :accessor sender-comm :initarg :sender)
   (address :accessor address :initarg :address))
  (:documentation "Condition zum automatisierten Weiterverarbeiten einer Sendung."))

(defmethod send ((comm stub-comm) address request-name &rest request-args)
  (verbose 1 (stub-comm-msg comm "sending ~s ~s to ~s" request-name request-args address))
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
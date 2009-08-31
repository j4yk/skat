(in-package skat-comm)

(defclass stub-comm (base-comm)
  ())

(export 'stub-comm 'skat-comm)

(defmethod start ((comm stub-comm))
  (values))

(defmethod login ((comm stub-comm) data)
  (values))

(defmethod send ((comm stub-comm) address request-name &rest request-args)
  (unless (typep address 'stub-comm)
    (error "stub-comm can only communicate to other stub-comms and the addresses are the comm-objects themselves."))
  (apply #'push-request address comm request-name request-args)
  (values))

(defmethod stop ((comm stub-comm))
  (values))
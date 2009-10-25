(in-package :skat-ui)

(defclass stub-ui (base-ui)
  ()
  (:documentation "Schritt- für Schritt UI, kann gar nichts."))

(defun stub-ui-msg (format-str &rest format-args)
  (apply #'format t (concatenate 'string "UI: " format-str) format-args)
  (terpri))

(defmethod main-loop-step ((ui stub-ui))
  "Schiebt den Kernel an, seine Requests abzuholen."
  (stub-ui-msg "doing step.")
  (kernel:receive-requests (kernel ui)))

(defmethod send-request-to-kernel ((ui stub-ui) request-name &rest arguments)
  "Sendet eine Anfrage an den Kernel, der sie in den meisten Fällen dann verschicken wird."
  (stub-ui-msg "sending ~a ~a to kernel" request-name arguments)
  (apply #'kernel:call-handler-fn (kernel ui) ui request-name arguments))

(defmethod start ((ui stub-ui) &optional to-be-ignored)
  "Tut eigentlich nichts..."
  (declare (ignore ui to-be-ignored))
  (stub-ui-msg "started.")
  (values))

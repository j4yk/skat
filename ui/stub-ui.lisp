(in-package :skat-ui)

(defclass stub-ui (base-ui)
  ()
  (:documentation "Schritt- für Schritt UI, kann gar nichts."))

(defmacro define-all-requests-handler ((ui-class data-argname) &body body)
  "Definiert einen Handler für alle Requests..."
  (cons 'progn
	(loop for request in requests::*request-definitions*
	   collect (let ((request-name (intern (symbol-name (car request))))
			 (parameters (mapcar #'(lambda (p) (intern (symbol-name p))) (cdr request))))
		     `(defhandler ,request-name (,ui-class ,@parameters)
			"Kollektiver Handler für alle Requests"
			(let ((,data-argname (list ',request-name ,@parameters)))
			  ,@body))))))
	  
(defun stub-ui-msg (format-str &rest format-args)
  (apply #'format t (concatenate 'string "~%UI: " format-str) format-args)
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

(define-all-requests-handler (stub-ui request-call)
  (stub-ui-msg "UI received ~a ~a from ~a" (car request-call) (cdr request-call) sender))
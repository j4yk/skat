(in-package :skat-ui)

(defclass stub-ui (base-ui)
  ((received-requests :accessor received-requests :initform nil))
  (:documentation "Schritt- für Schritt UI, kann gar nichts."))

(defvar *stub-ui-verbosity* 1 "Regelt die Gesprächigkeit aller stub-uis")

(defmacro verbose (min-level &body body)
  "Führt body nur aus, wenn *stub-ui-verbosity* mindestens min-level ist"
  `(when (>= *stub-ui-verbosity* ,min-level)
     ,@body))

(defmethod print-object ((ui stub-ui) stream)
  (print-unreadable-object (ui stream :type t)
    (princ "of " stream)
    (if (slot-boundp ui 'kernel)
	(princ (kernel ui) stream)
	(princ "NO KERNEL BOUND" stream))))

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
	  
(defun stub-ui-msg (ui format-str &rest format-args)
  (apply #'format t (concatenate 'string "~%~a: " format-str) ui format-args)
  (terpri))

(defmethod main-loop-step ((ui stub-ui))
  "Schiebt den Kernel an, seine Requests abzuholen."
  (verbose 1 (stub-ui-msg ui "doing step."))
  (kernel:receive-requests (kernel ui)))

(defmethod just-one-step ((ui stub-ui))
  (main-loop-step ui))

(defmethod send-request-to-kernel ((ui stub-ui) request-name &rest arguments)
  "Sendet eine Anfrage an den Kernel, der sie in den meisten Fällen dann verschicken wird."
  (verbose 1 (stub-ui-msg ui "sending ~s ~s to kernel" request-name arguments))
  (apply #'kernel:call-handler-fn (kernel ui) ui request-name arguments))

(defmethod start ((ui stub-ui) &optional to-be-ignored)
  "Tut eigentlich nichts...
to-be-ignored, da host-ui bspw. einen zweiten Parameter nimmt"
  (declare (ignore to-be-ignored))
  (verbose 1 (stub-ui-msg ui "started."))
  (values))

(define-condition stub-ui-request-arrived ()
  ((ui :accessor ui :initarg :ui)
   (request-call :accessor request-call :initarg :request-call))
  (:documentation "Condition für den Fall, dass eine Stub-UI etwas empfangen hat."))

(define-all-requests-handler (stub-ui request-call)
  (verbose 1 (stub-ui-msg ui "received ~s ~s from ~s" (car request-call) (cdr request-call) sender))
  (setf (received-requests ui) (nconc (received-requests ui) (list request-call))) ; die Anfrage notieren
  (restart-case (signal 'stub-ui-request-arrived :ui ui :request-call request-call)
    (continue () t)))

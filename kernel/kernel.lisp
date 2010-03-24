(in-package :skat-kernel)

(eval-when (:compile-toplevel)
  (defvar *kernel-states* nil
    "Liste Kernel-Klassenname > States; wird zum Überprüfen bei define-state-switch-function benutzt")

  (defun state-is-valid-p (state kernel-classname)
    "Überprüft, ob state ein Zustand für diese Kernel-Klasse ist."
    (member state (cdr (assoc kernel-classname *kernel-states*)))))

(defclass kernel ()
  ((ui :accessor ui :initarg :ui)
   (comm :accessor comm :initarg :comm)
   (state :accessor state :initform 'start)
   (deferred-requests :accessor deferred-requests :initform nil)
   (own-address :accessor own-address)))

(defmacro defkernel (name (&rest states) direct-slots &rest options)
  "Definiert eine neue Kernelklasse"
  (eval-when (:compile-toplevel)
    (push (cons name states) *kernel-states*)) ; für define-state-switch-function
  `(defclass ,name (kernel)
     ((valid-states :reader valid-states :allocation :class
		    :initform ',states)
      ,@direct-slots)
     ,@options))

(defvar *kernel-verbosity* #+debug 1 #-debug 0 "Wie viel die Kernels drucken")

(defmacro verbose (min-value &body body)
  "Führt body nur aus, wenn *kernel-verbosity* mindestens min-value ist."
  `(when (>= *kernel-verbosity* ,min-value)
     ,@body))

(defmethod print-object ((kernel kernel) stream)
  "Druckt den Zustand mit aus."
  (print-unreadable-object (kernel stream :type t :identity t)
    (princ "State=" stream)
    (princ (state kernel) stream)
    (when (slot-boundp kernel 'own-address)
      (format stream " Address=\"~a\"" (own-address kernel)))))

(defmacro defkernelmethod (name (kernel-class &rest method-parameters) &body body)
  "Definiert eine neue Methode für eine Kernelklasse,
in deren body eine lexikalische Bindung der Variable kernel
etabliert wird."
  (multiple-value-bind (forms docstring declarations) (parse-function-body body) ; docstring finden
    `(defmethod ,name ((,kernel-class ,kernel-class) ,@method-parameters)
       ,docstring
       ,declarations
       (let ((kernel ,kernel-class))	; kernel binden
	 (declare (ignorable kernel))	; muss aber nicht zwangsläufig benutzt werden
	 ,@forms))))

(defun call-ui (request-name kernel sender &rest request-args)
  "Stellt die Anfrage an die UI weiter."
  (apply (skat-ui:handler-fn request-name) (ui kernel) sender request-args))

(define-condition invalid-request-sender-error (error)
  ((sender :accessor sender :initarg :sender)
   (kernel :accessor kernel :initarg :kernel)
   (expected-senders :accessor expected-senders :initarg :expected-senders)
   (request-name :accessor request-name :initarg :request-name)))

(defmacro with-correct-sender (sender correct-senders &body body)
  "Führt body nur aus, wenn sender und correct-sender übereinstimmen.
Andernfalls wird ein invalid-request-sender-error signalisiert.
Dieses Makro sollte nur innerhalb von defhandler verwendet werden,
da es die Bindungen der Variablen kernel und request-name voraussetzt.
Error Conditions: invalid-request-sender-error"
  `(if (handler-bind ((unbound-slot #'(lambda (condition) (use-value nil condition)))) ; nicht verzweifeln, wenn Slots ungebunden sind
	 (member sender (list ,@correct-senders) :test (address-compare-function kernel)))
       (progn ,@body)
       (error 'invalid-request-sender-error :sender ,sender :kernel kernel :expected-senders (list ,@correct-senders)
	      :request request-name)))

(defmethod address-compare-function ((kernel kernel))
  (comm:address-compare-function (comm kernel)))

(defmethod address-equal ((kernel kernel) address1 address2)
  "Gibt t zurück, wenn die Adressen laut address-compare-function gleich sind."
  (funcall (address-compare-function kernel) address1 address2))

(defmethod defer-request ((kernel kernel) request-name sender request-args tries)
  "Appends the request to (deferred-requets kernel)"
  (if (null (deferred-requests kernel))
      (push (list request-name sender request-args tries)
	    (deferred-requests kernel))
      (setf (cdr (last (deferred-requests kernel)))
	    (list (list request-name sender request-args tries)))))

(defmethod get-deferred-request ((kernel kernel))
  "Pops a request from (deferred-requests kernel) and returns it with (values-list)"
  (values-list (pop (deferred-requests kernel))))

(defmethod has-deferred-request-p ((kernel kernel))
  (not (null (deferred-requests kernel))))

(defun skip-request-restart (request-name sender)
  (warn "skipped request ~a from ~a" request-name sender))

(defun skip-not-defined-request (request-name sender)
  (warn "skipping unknown request ~a from ~a" request-name sender)
  (invoke-restart 'skip-request))

(defun skip-request-from-invalid-sender (request-name sender)
  (warn "skipping not allowed request ~a from ~a" request-name sender)
  (invoke-restart 'skip-request))

(defmethod receive-requests ((kernel kernel))
  "Holt alle vorliegenden Anfragen aus dem Kommunikationsobjekt heraus und ruft entsprechende Anfragehandler auf."
  (let ((defer nil))
    (loop while (or (has-deferred-request-p kernel) (comm:has-request (comm kernel)))
       do (multiple-value-bind (request-name sender request-args tries)
	      (if defer
		  ;; don't get the deferred request instantly again
		  (progn (setf defer nil) (comm:get-request (comm kernel)))
		  ;; get deferred ones first, then freshly arrived
		  (let ((r (multiple-value-list (get-deferred-request kernel))))
		    (if r
			(values-list r)
			(comm:get-request (comm kernel)))))
	    (verbose 1 (format *debug-io* "~%kernel: processing ~a ~a from ~a, try ~a"
			       request-name request-args sender (or tries 1)))
	    (restart-case (when (> (or tries 1) 5)
			    (error "Request ~a ~a from ~a signalled an error already ~a times!"
				   request-name request-args sender (1- tries)))
	      (retry () :report "Call the kernel handler again anyway"))
	    (unless (null request-name)
	      ;; call the handler function and provide some restarts
	      (restart-case
		  (let ((handler (handler-fn request-name)))
		    ;; if no such function exists, ignore this request or whatever it is
		    (when handler
		      ;; if handler is not implemented for this kernel, ignore it
		      (handler-bind ((handler-not-defined-error (lambda () (skip-not-defined-request request-name sender)))
				     (invalid-request-sender-error (lambda () (skip-request-from-invalid-sender request-name sender))))
			(apply handler kernel sender request-args))))
		(retry (&optional condition)
		  :report "Call the kernel handler again immediately"
		  (declare (ignore condition))
		  (comm::prepend-request (comm kernel) sender request-name request-args))
		(retry-later (&optional condition)
		  :report "Call the kernel handler again after another request has been processed"
		  (declare (ignore condition))
		  (defer-request kernel request-name sender request-args (if tries (1+ tries) 1))
		  (setf defer t))
		(skip-request (&optional condition) :report "Skip this request"
			      (declare (ignore condition))
			      (skip-request-restart request-name sender))
		(continue (&optional condition)
		  :report "Skip this request (equal to skip-request)"
		  (declare (ignore condition))
		  (skip-request-restart request-name sender))))))))

(define-condition invalid-kernel-state-error (error)
  ((kernel-class :accessor kernel-class :initarg :kernel-class)
   (target-state :accessor target-state :initarg :target-state))
  (:documentation "Condition in der Versucht wurde, einen Kernel in einen für ihn nicht vorgesehenen
Zustand wechseln zu lassen"))

(defmethod switch-state ((kernel kernel) target-state)
  "Wechselt den Zustand des Kernelobjekts. Dies hat Auswirkungen auf die Menge der akzeptierten Anfragen.
Diese Methode verändert nur den Slot, führt aber keine mit dem Wechsel assoziierten Aktionen aus.
Siehe [[define-state-switch-function]].
Error Conditions: invalid-kernel-state-error"
  (if (member target-state (valid-states kernel))
      (setf (state kernel) target-state)
      (error 'invalid-kernel-state-error :kernel-class (class-of kernel) :target-state target-state)))

;; Comm teilt die eigene Adresse nach erfolgtem Login mit
(defhandler own-address (unregistered) comm (kernel address)
  (setf (own-address kernel) address)
  (call-ui 'own-address kernel kernel address))

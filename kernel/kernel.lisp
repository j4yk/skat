(in-package :skat-kernel)

(defclass kernel ()
  ((ui :accessor ui :initarg :ui)
   (comm :accessor comm :initarg :comm)
   (state :accessor state :initform 'start)
   (own-address :accessor own-address)))

(defmacro defkernel (name (&rest states) direct-slots &rest options)
  `(defclass ,name (kernel)
     ((valid-states :reader valid-states :allocation :class
		    :initform ',states)
      ,@direct-slots)
     ,@options))

(defmethod print-object ((kernel kernel) stream)
  "Druckt den Zustand mit aus."
  (print-unreadable-object (kernel stream :type t :identity t)
    (princ "State=" stream)
    (princ (state kernel) stream)))

(defmacro defkernelmethod (name (kernel-class &rest method-parameters) &body body)
  "Definiert eine neue Methode für eine Kernelklasse,
in deren body eine lexikalische Bindung der Variable kernel
etabliert wird."
  (multiple-value-bind (forms docstring) (parse-function-body body) ; docstring finden
    `(defmethod ,name ((,kernel-class ,kernel-class) ,@method-parameters)
       ,docstring
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
  "Führt body nur aus, wenn sender und corretct-sender übereinstimmen.
Andernfalls wird ein invalid-request-sender-error signalisiert.
Dieses Makro sollte nur innerhalb von defhandler verwendet werden,
da es die Bindungen der Variablen kernel und request-name voraussetzt."
  `(if (member sender correct-senders :test (address-compare-function kernel))
       (progn ,@body)
       (error 'invalid-request-sender-error :sender ,sender :kernel kernel :expected-senders (list ,@correct-senders)
	      :request request-name)))

(defmethod address-compare-function ((kernel kernel))
  (comm:address-compare-function (comm kernel)))

(defmethod receive-requests ((kernel kernel))
  "Holt alle vorliegenden Anfragen aus dem Kommunikationsobjekt heraus und ruft entsprechende Anfragehandler auf."
  (loop while (comm:has-request (comm kernel))
     do (multiple-value-bind (request-name sender request-args) (comm:get-request (comm kernel))
	  (format *debug-io* "processing ~a (~{~a ~}) from ~a" request-name request-args sender)
	  (apply (handler-fn request-name) kernel sender request-args))))

(define-condition invalid-kernel-state-error (error)
  ((kernel-class :accessor kernel-class :initarg :kernel-class)
   (target-state :accessor target-state :initarg :target-state))
  (:documentation "Condition in der Versucht wurde, einen Kernel in einen für ihn nicht vorgesehenen
Zustand wechseln zu lassen"))

(defmethod switch-state ((kernel kernel) target-state)
  "Wechselt den Zustand des Kernelobjekts. Dies hat Auswirkungen auf die Menge der akzeptierten Anfragen."
  (if (member target-state (valid-states kernel))
      (setf (state kernel) target-state)
      (error 'invalid-kernel-state-error :kernel-class (class-of kernel) :target-state target-state)))

;; Comm teilt die eigene Adresse nach erfolgtem Login mit
(defhandler own-address (unregistered) (kernel address)
  (setf (own-address kernel) address))

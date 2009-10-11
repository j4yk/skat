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

(defmacro call-ui (request-name player sender &rest request-args)
  "Stellt die Anfrage an die UI weiter."
  `(apply #',(intern (symbol-name (handler-fn-name request-name)) 'skat-ui) (ui ,player) ,sender ,@request-args))

(defmethod address-compare-function ((kernel kernel))
  (address-compare-function (comm kernel)))

(defmethod receive-requests ((kernel kernel))
  "Holt alle vorliegenden Anfragen aus dem Kommunikationsobjekt heraus und ruft entsprechende Anfragehandler auf."
  (loop while (comm:has-request (comm kernel))
     do (multiple-value-bind (request-name sender request-args) (comm:get-request (comm kernel))
	  (apply (handler-fn request-name) sender request-args))))

(define-condition invalid-kernel-state-error (error)
  ((kernel-class :accessor kernel-class :initarg :kernel-class)
   (target-state :accessor target-state :initarg :target-state))
  (:documentation "Condition in der Versucht wurde, einen Kernel in einen für ihn nicht vorgesehenen
Zustand wechseln zu lassen"))

(defmethod switch-state ((kernel kernel) target-state)
  "Wechselt den Zustand des Kernelobjekts. Dies hat Auswirkungen auf die Menge der Akzeptierten Anfragen."
  (if (member target-state (valid-states kernel))
      (setf (state kernel) target-state)
      (error 'invalid-kernel-state-error :kernel-class (class-of kernel) :target-state target-state)))

;; Comm teilt die eigene Adresse nach erfolgtem Login mit
(defhandler own-address (unregistered) (kernel comm address)
  (setf (own-address kernel) address))

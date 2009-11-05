(in-package skat-kernel)

(defun make-stub-host ()
  (let ((host (make-instance 'host :ui (make-instance 'ui::stub-ui) :comm (make-instance 'comm::stub-comm)
			     :login-data nil)))
    (setf (ui::kernel (ui host)) host)
    (comm:start (comm host))
    (ui:start (ui host))
    host))

(defmacro mkstubhost ()
  `(progn
     (defparameter host (make-stub-host))
     (defparameter ui (ui host))
     (defparameter comm (comm host))
     (values 'host 'ui 'comm)))

(defun ui-step ()
  (ui::main-loop-step ui))

(defmacro push-request (from name &rest args)
  `(comm::push-request comm ,from ',name ,@args))

(defmacro process-request (from name &rest args)
  `(progn
     (push-request ,from ,name ,@args)
     (ui-step)))

(in-package skat-kernel)

(defun make-stub-host ()
  (let ((host (make-instance 'host :ui (make-instance 'ui::stub-ui) :comm (make-instance 'comm::stub-comm)
			     :login-data nil)))
    (setf (ui::kernel (ui host)) host)
    (comm:start (comm host))
    (ui:start (ui host))
    host))

(defun standard-xmpp-credentials (resource)
  (comm::make-xmpp-login-data "jakob" "localhost" "localhost" resource "habaladings" :sasl-plain))

(defun standard-registration-data ()
  (comm::make-xmpp-registration-data "jakob@localhost/host"))

(defun make-host (ui-class comm-class &optional (login-data (standard-xmpp-credentials "host")))
  (let ((host (make-instance 'host :ui (make-instance ui-class) :comm (make-instance comm-class)
			     :login-data login-data)))
    (setf (ui::kernel (ui host)) host)
    host))

(defun make-player (ui-class comm-class)
  (let ((p (make-instance 'player :ui (make-instance ui-class) :comm (make-instance comm-class))))
    (setf (ui::kernel (ui p)) p)
    p))




(defparameter host nil)
(defparameter ui nil)
(defparameter comm nil)
(defparameter cards1 nil)
(defparameter cards2 nil)
(defparameter cards3 nil)

(defmacro mkstubhost ()
  `(progn
     (defparameter host (make-stub-host))
     (defparameter ui (ui host))
     (defparameter comm (comm host))
     (values 'host 'ui 'comm)))

(defmacro with-debug-handlers (&body body)
  `(handler-bind ((comm::stub-communication-send
		   #'(lambda (send)
		       (case (comm::request-name send)
			 (cards (case (comm::address send)
				  (1 (setq cards1 (sort-cards (car (comm::args send)) :grand)))
				  (2 (setq cards2 (sort-cards (car (comm::args send)) :grand)))
				  (3 (setq cards3 (sort-cards (car (comm::args send)) :grand))))
				(format t "*** modified cards for ~a~%" (comm::address send)))
			 (skat (macrolet ((setq-cards (var)
					    `(setq ,var (sort-cards (nconc ,var (car (comm::args send))) :grand))))
				 (format t "*** modifying cards for ~a~%" (comm::address send))
				 (case (comm::address send)
				   (1 (setq-cards cards1))
				   (2 (setq-cards cards2))
				   (3 (setq-cards cards3))))))
		       (continue))))
     ,@body))

(defun ui-step ()
  (assert (typep ui 'ui::base-ui) (ui))
  (with-debug-handlers 
      (ui::main-loop-step ui)))

(defmacro push-request (from name &rest args)
  `(comm::push-request comm ,from ',name ,@args))

(defmacro process-request (from name &rest args)
  `(progn
     (push-request ,from ,name ,@args)
     (ui-step)
     ,(case name
	(card
	 `(macrolet ((delete-card (var)
		       `(setq ,var (delete ,',(car args) ,var :test #'equalp))))
	    ,(case from
	      (1 `(delete-card cards1))
	      (2 `(delete-card cards2))
	      (3 `(delete-card cards3)))))
	(skat `(macrolet ((delete-cards (var)
			    ,(destructuring-bind (quote (a b)) (car args)
						 (declare (ignore quote))
						 ``(setq ,var (delete ,',a (delete ,',b ,var :test #'equalp) :test #'equalp)))))
		 ,(case from
		   (1 `(delete-cards cards1))
		   (2 `(delete-cards cards2))
		   (3 `(delete-cards cards3))))))))

(in-package skat-kernel)

(defun make-test-host ()
  (let ((host (make-instance 'host :ui (make-instance 'ui:host-ui) :comm (make-instance 'comm::stub-comm) :login-data nil)))
    (setf (ui::kernel (ui host)) host)
    host))

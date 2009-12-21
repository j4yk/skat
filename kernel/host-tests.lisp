(in-package skat-kernel)

(defun make-test-host ()
  (let ((host (make-instance 'host :ui (make-instance 'ui:host-ui) :comm (make-instance 'comm::stub-comm) :login-data nil)))
    (setf (ui::kernel (ui host)) host)
    (comm:start (comm host))		; Comm starten
    host))

(defun update-kernels (kernels)
  "Lässt jede UI ausstehende Anfragen verarbeiten"
  (handler-bind ((comm::stub-communication-send #'stub-communication-send-testhandler))
    (dolist (kernel kernels)
      (ui:just-one-step (ui kernel)))))
  
(in-package comm)

(defun test-login-data (&optional (name "skat") (resource "skat"))
  (make-xmpp-login-data :username name :password "skat"
			:hostname "draugr.de" :domain ""
			:resource resource :mechanism :sasl-plain))

(in-package kern)

(defun test-host-login-data ()
  (comm::make-xmpp-login-data :username "skat" :password "skat"
			      :hostname "draugr.de" :domain ""
			      :resource "host" :mechanism :sasl-plain))

(in-package comm)

(defun test-login-data (&optional (name "skat") (resource "skat"))
  (make-xmpp-login-data :username name :password "skat"
			:hostname "draugr.de" :domain ""
			:resource resource :mechanism :sasl-plain))

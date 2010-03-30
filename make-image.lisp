
(eval-when (:execute)

  (handler-bind ((error (lambda (c)
			  (let ((cl:*print-escape* nil))
			    (format t "Error: ")
			    (print-object c *standard-output*)
			    (terpri))
			  (sb-ext:quit :unix-status 1))))
    (require 'skat))

  (sb-ext:save-lisp-and-die "skat.core"))

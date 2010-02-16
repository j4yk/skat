#+win32
(progn
  (defun set-default-external-format (external-format)
    (assert (sb-impl::find-external-format external-format))
    (setf sb-impl::*default-external-format* external-format)
    (with-output-to-string (*error-output*)
      (setf sb-sys:*stdin*
	    (sb-sys:make-fd-stream 0 :name "standard input" :input t :buffering :line))
      (setf sb-sys:*stdout*
	    (sb-sys:make-fd-stream 1 :name "standard output" :output t :buffering :line))
      (setf sb-sys:*stderr*
	    (sb-sys:make-fd-stream 2 :name "standard error" :output t :buffering :line))
      (setf sb-sys:*tty* (make-two-way-stream sb-sys:*stdin* sb-sys:*stdout*))
      (princ (get-output-stream-string *error-output*) sb-sys:*stderr*))
    (values))

  (set-default-external-format :utf-8)

  ;; test:
  (coerce (mapcar #'code-char '(40 105 110 115 116 97 110 99 101 32 12383 12429 12358 32 40 97 116 45
				109 111 115 116 32 49 32 12365 12423 12358 12384 12356 12434 25345
				12388 41 41) ) 'string))
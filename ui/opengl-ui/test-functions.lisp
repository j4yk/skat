(in-package gl-ui)

;; Testing routines that are not modules but just to be tried at the REPL go here

(cffi:defcallback say-hello :void ((event ag::event))
  (let ((s (ag::event-string event 1))
	(new-state (ag::event-int event 2)))
    (print s)
    (ag::text-msg :info "Hello, ~a! (state = ~a)" s new-state)))

(defun test-agar-button ()
  (let* ((win (ag:window-new))
	 (btn (ag::button-new win 0 "Say Hello")))
    (ag:window-show win)
    (cffi:with-foreign-string (s "World")
      (ag::set-event btn "button-pushed" (cffi:callback say-hello) "%s" :pointer s)
      (ag::event-loop))))
(in-package gl-ui)

;; Testing routines that are not modules but are just to be tried at the REPL go here

(cffi:defcallback say-hello :void ((event ag:event))
  (let ((s (ag:event-string event 1))
	(new-state (ag:event-int event 2)))
    (print s)
    (ag:text-msg :info "Hello, ~a! (state = ~a)" s new-state)))

(defun test-agar-button ()
  (let* ((win (ag:window-new))
	 (btn (ag:button-new win 0 "Say Hello")))
    (ag:window-show win)
    (cffi:with-foreign-string (s "World")
      (ag:set-event btn "button-pushed" (cffi:callback say-hello) "%s" :pointer s)
      (ag:event-loop))))

(defun test-agar-and-other-drawings ()
  "This tests how Agar works without it's own event loop
Works."
  (let ((mvm nil) (pjm nil) (txm nil))
    (labels ((display-bg ()
	       (gl:matrix-mode :modelview)
	       (gl:with-pushed-matrix
		 (gl:load-identity)
		 (gl:translate 0 0 -10)
		 (gl:color 1 0 0)
		 (gl:with-primitives :polygon
		   (gl:vertex 0 0 0)
		   (gl:vertex 2 -1 -3)
		   (gl:vertex 3 0 5)
		   (gl:vertex -1 5 3)
		   (gl:vertex 0 1 0))))
	     (resize-bg ()
	       (gl:matrix-mode :texture) (gl:push-matrix) (gl:load-identity)
	       (gl:matrix-mode :modelview) (gl:push-matrix) (gl:load-identity)
	       (gl:matrix-mode :projection)
	       (gl:push-matrix)
	       (gl:load-identity)
	       (glu:perspective 60.0 (/ 640 480) 0.1 100.0)
	       (setf mvm (gl:get-float :modelview-matrix))
	       (setf pjm (gl:Get-float :projection-matrix))
	       (setf txm (gl:get-float :texture-matrix))
	       (gl:matrix-mode :projection) (gl:pop-matrix)
	       (gl:matrix-mode :modelview) (gl:pop-matrix)
	       (gl:matrix-mode :texture) (gl:pop-matrix))
	     (draw-bg ()
	       (gl:matrix-mode :texture) (gl:push-matrix) (gl:load-matrix txm)
	       (gl:matrix-mode :projection) (gl:push-matrix) (gl:load-matrix pjm)
	       (gl:matrix-mode :modelview) (gl:push-matrix) (gl:load-matrix mvm)
	       (gl:push-attrib :all-attrib-bits)
	       (gl:enable :cull-face)
	       (gl:disable :clip-plane0 :clip-plane1 :clip-plane2 :clip-plane3)
	       (display-bg)
	       (gl:pop-attrib)
	       (gl:matrix-mode :modelview) (gl:pop-matrix)
	       (gl:matrix-mode :texture) (gl:pop-matrix)
	       (gl:matrix-mode :projection) (gl:pop-matrix)))
      (resize-bg)
      (let* ((win (ag:window-new))
	     (label (ag:label-new-string win "Testfenster")))
	(declare (ignorable label))
	(ag:window-show win)
	(dotimes (n 5)
	  (let ((win (ag::window-new)))
	    (ag:window-set-caption win (format nil "TL~a" n))
	    (ag:label-new-string win (format nil "Top Left ~a" n))
	    (ag:set-window-position win :tl t)
	    (ag:window-show win)))
	(sdl:with-events (:poll ev)
	  (:quit-event nil t)
	  (:mouse-button-down-event ()
				    (ag:process-event ev))
	  (:mouse-button-up-event ()
				  (ag:process-event ev))
	  (:mouse-motion-event ()
			       (ag:process-event ev))
	  (:resize-event ()
			 (resize-bg)
			 (ag:process-event ev))
	  (:idle ()
		 (when (ag:timeouts-queued-p)
		   (ag:process-timeouts (sdl:system-ticks)))
		 (ag:render
		   (draw-bg)
		   ;(ag::lock-vfs ag::*view*)
		   (dolist (win (ag:tailqueue-to-list (ag:windows ag:*view*) #'ag:next-window))
		     ;; (ag::lock-object win)
		     (ag:window-draw win)
		     ;; (ag::unlock-object win)
		     )
		   ;(ag::unlock-vfs ag::*view*)
		   )))))))
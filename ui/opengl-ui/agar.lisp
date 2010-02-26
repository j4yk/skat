(in-package gl-ui)

(defclass agar (module)
  ()
  (:documentation "Module that cares for Agar's affairs, i. e. handles events"))

(defmethod handle-event ((module agar) sdl-event)
  #+agar
  (ag:process-event sdl-event))

(defmethod draw ((module agar))
  "Draw Agar's windows and process Timeouts"
  #+agar
  (when (ag::timeouts-queued-p)
    (ag::process-timeouts (sdl:system-ticks)))
  #+agar
  (let ((winlist (ag::tailqueue-to-list (ag::windows ag::*view*) #'ag::next-window)))
    (dolist (win winlist)
      (unless (cffi:null-pointer-p win)
	(ag:window-draw win)))))

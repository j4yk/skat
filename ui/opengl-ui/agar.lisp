(in-package gl-ui)

(defclass agar (module)
  ()
  (:documentation "Module that cares for Agar's affairs, i. e. handles events"))

(defmethod handle-event ((module agar) sdl-event)
  #+agar
  (ag:process-event sdl-event))

(defmethod draw ((module agar))
  "Draw Agar's windows"      
  #+agar
  (let ((win (ag::tailqueue-first (ag::windows ag::*view*))))
    ;; TODO: render the other windows as well....
    (unless (cffi:null-pointer-p win)
      (ag:window-draw win))))
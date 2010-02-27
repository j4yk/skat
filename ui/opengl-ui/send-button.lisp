(in-package gl-ui)

(defclass send-button (module)
  ((window) (button)
   (click-handler :accessor click-handler-function)))

(defvar *send-button-table* (make-hash-table)
  "This table is used to dispatch the button pointer to the module")

(cffi:defcallback send-button-pushed :void ((event ag::event))
  (let* ((btn-pointer (ag:event-self event))
	 (module (gethash (cffi:pointer-address btn-pointer) *send-button-table*)))
    (if module
	(funcall (click-handler-function module))
	(warn "No module to send button found!"))))

(defmethod setup-event-handlers ((module send-button))
  (with-slots (button) module
    (ag:set-event button "button-pushed" (cffi:callback send-button-pushed) "")))

(defmethod initialize-instance :after ((module send-button) &key)
  "Creates the Agar window with the button"
  ;; allocate a foreign pointer... what is allocated is equal
  ;; important is the SAP that foreign-alloc returns
  (with-slots (window button) module
    (setf window (ag:window-new :noborders :notitle))
    (setf button (ag:button-new window 0 "Send"))
    (setup-event-handlers module)
    (setf (gethash (cffi:pointer-address button) *send-button-table*) module) ; setup dispatch path from button SAP to module
    (ag:window-show window)))

(defmethod cleanup ((module send-button))
  (with-slots (window button) module
    (ag:hide-window window)
    (ag:detach-object window)
    (remhash (cffi:pointer-address button) *send-button-table*)))

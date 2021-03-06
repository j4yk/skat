(in-package gl-ui)

(defvar *windows-to-be-autosized* nil)

(defun autosize-windows ()
  (loop until (null *windows-to-be-autosized*)
     do (ag:set-window-geometry (pop *windows-to-be-autosized*) -1 -1 -1 -1)))

(defun enqueue-window-autosize (window)
  (check-type window foreign-pointer)
  (push window *windows-to-be-autosized*))

(defclass agar (module)
  ()
  (:documentation "Module that cares for Agar's affairs, i. e. handles events"))

(defmethod handle-event ((module agar) sdl-event)
  (declare (optimize speed))
  (ag:process-event sdl-event))

(defmethod draw ((module agar))
  "Draw Agar's windows and process Timeouts"
  (declare (optimize speed))
  (when (ag:timeouts-queued-p)
    (ag:process-timeouts (sdl:system-ticks)))
  (let ((winlist (ag:tailqueue-to-list (ag:windows ag:*view*) #'ag:next-window)))
    (dolist (win winlist)
      (unless (cffi:null-pointer-p win)
	(ag:window-draw win))))
  (autosize-windows))


;; some utility functions for handling agar

(defstruct foreign-variable "Stores a foreign pointer and optionally its size (buffer)" ptr size)

(defclass agar-window ()
  ((window :accessor window)
   (module :accessor module :initarg :module))
  (:documentation "Can be used to have a collection of foreign pointers to widgets of the window.
Meant to be subclassed."))

(defmethod show ((agar-window agar-window))
  "Calls AG_WindowShow on the Agar window"
  (ag:window-show (window agar-window)))

(defmethod hide ((agar-window agar-window))
  (ag:hide-window (window agar-window)))

(defmethod autosize ((agar-window agar-window))
  (enqueue-window-autosize (window agar-window)))

(defmacro callback (name return-type args &body body)
  "Defines and returns the pointer to a callback function"
  `(progn
     (defcallback ,name ,return-type ,args ,@body)
     (cffi:callback ,name)))

(defmacro event-handler (function-name)
  "Specifically creates a callback that can be used as an Agar event handler function"
  (if (symbolp function-name)
      `(callback ,function-name :void ((event ag:event))
	 (,function-name event))
      (let ((callback-name (gensym "CALLBACK")))
	`(callback ,callback-name :void ((event ag:event))
	   (funcall ,function-name event)))))

(defmacro lambda-event-handler (event-var &body body)
  "Creates a foreign callback in the current lexical environment
with return type void and a single ag:event argument with the name event-var
and body as the function body (declarations are useless)."
  (let ((callback-name (gensym "CALLBACK")))
    `(callback ,callback-name :void ((,event-var ag:event))
       ,@body)))

(defmacro std-event-handler (&body body)
  "Creates a lambda event handler that ignores the event structure"
  (let ((event (gensym "EVENT")))
    `(lambda-event-handler ,event
       (declare (ignore ,event))
       ,@body)))

(defmacro lambda-timeout-callback ((object ival arg) &body body)
  "Creates a foreign callback in the current lexucal environment with
  return type Uint32 and the arguments for an Agar timeout callback as
  defined in AG_Timeout(3).  The names of these arguments are given by
  object, ival and arg."
  (let ((callback-name (gensym "CALLBACK")))
    `(progn
       (ag:define-timeout-callback ,callback-name (,object ,ival ,arg)
	 ,@body)
       (cffi:callback ,callback-name))))

(defun modal-windows-visible-p ()
  (> (ag:agar-list-count (ag:lmodal ag:*view*)) 0))

(defun get-rid-of-window (win)
  "Hides and detaches the window"
  (ag:hide-window win)
  (ag:detach-object win))

(defun ensure-detached (object parent)
  "Detaches an object of its parent object is parent"
  (when (pointer-eq (ag:parent-object object) parent)
    (ag:detach-object object)))

(defun ensure-attached (object parent)
  "Attaches an object to parent if it is not already attached to parent"
  (declare (optimize debug))
  (unless (pointer-eq (ag:parent-object object) parent)
    (ag:attach-object parent object)))

(defmacro let*-slots (instance let-bindings &body body)
  "Expands into a with-slots form with the let-varnames as the slot names
and the slots being setf-ed to the supplied let-values"
  `(with-slots ,(mapcar #'car let-bindings) ,instance
     ,@(mapcar #'(lambda (let-binding-form)
		   (destructuring-bind (name val) let-binding-form
		     `(setf ,name ,val)))
	       let-bindings)
     ,@body))

(defmacro alloc-finalized (object type &rest foreign-alloc-args &key initial-element initial-contents (count 1) null-terminated-p)
  "Allocate foreign memory that is to be freed together with object"
  (declare (ignorable initial-element initial-contents count null-terminated-p))
  `(let ((ptr (foreign-alloc ,type ,@foreign-alloc-args)))
     (prog1 ptr (trivial-garbage:finalize ,object #'(lambda () (foreign-free ptr))))))

(defmacro alloc-finalized-string (object string)
  "Allocate memory for this specific string and have it freed later together with object"
  `(let ((ptr (cffi:foreign-string-alloc ,string)))
     (prog1 ptr (trivial-garbage:finalize ,object #'(lambda () (foreign-free ptr))))))

(defun foreign-string-size (string)
  (with-foreign-string ((ptr size) string)
    size))

(defmacro expanded-h (widget)
  "Call ag:expand-horiz on widget and return widget"
  `(let ((wid ,widget))
     (prog1 wid (ag:expand-horiz wid))))

(defmacro expanded-v (widget)
  "Call ag:expand-vert on widget and return widget"
  `(let ((wid ,widget))
     (prog1 wid (ag:expand-vert wid))))  

(defmacro expanded (widget)
  "Call ag:expanded on widget and return widget"
  `(let ((wid ,widget))
     (prog1 wid (ag:expand wid))))


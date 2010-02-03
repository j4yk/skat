(in-package skat-opengl-ui)

(defclass login-and-register-module (module)
  ((state :accessor state :initform 'enter-login)
   (login-struct-type :accessor login-struct-type :initarg :login-struct-type)
   (login-window :accessor login-window)
   (registration-struct :accessor registration-struct)
   (register-window :accessor register-window))
  (:documentation "Modul für die Einwähl- und Registrier-Prozedur"))

(defun init-login-window (module)
  "Erstelle das Login-Daten-Fenster und zeige es an."
  (let ((window (agar:window-new :modal :noclose)))
    (setf (login-window module) window)
    (agar:window-set-caption window "Login")
    (agar:label-new-string window "Hallöle")
    (agar::text-msg :info "bar")
    (agar:window-show window)))

(defmethod initialize-instance :after ((module login-and-register-module)
				       &rest initargs
				       &key
				       &allow-other-keys)
  (declare (ignore initargs))
  (init-login-window module))

(defmethod draw ((module login-and-register-module))
  "Zeichne das Logindaten- oder Registrierungsdaten-Fenster"
  ;; zeichne deine Fenster
  (case (state module)
    (enter-login (agar:window-draw (login-window module)))))

(defmethod handle-event ((module login-and-register-module) event)
  ;; handle event
  (agar:process-event event))

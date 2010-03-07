(in-package gl-ui)

(defclass notifications (module)
  ((players-module :initarg :players-module)))

(defmethod show-declarer ((module notifications) declarer-address)
  (with-slots (players-module) module
    (ag:text-msg :info "~a ist der Alleinspieler dieses Spiels"
		 (player-name players-module declarer-address))))

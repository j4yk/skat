(in-package gl-ui)

(defclass game-declaration (module)
  ())

(defmethod take-skat ((module game-declaration))
  "Send hand-decision to kernel, taking the skat."
  (call-kernel-handler (ui module) 'hand-decision nil))

(defmethod play-hand ((module game-declaration))
  "Send hand-decision to kernel, playing a hand game."
  (call-kernel-handler (ui module) 'hand-decision t))  

(defmethod query-hand ((module game-declaration))
  (ag:prompt-options "Skat aufnehmen?"
		     (list "Aufnehmen" (lambda-event-handler event
					 (take-skat module)
					 (get-rid-of-window (ag:event-ptr event 1))))
		     (list "Hand spielen" (lambda-event-handler event
					    (play-hand module)
					    (get-rid-of-window (ag:event-ptr event 1)))))
  (values))

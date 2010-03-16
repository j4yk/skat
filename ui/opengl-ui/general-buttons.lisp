(in-package gl-ui)

(defclass general-buttons-window (agar-window)
  ((show-last-trick-btn)
   (leave-btn)))

(defmethod initialize-instance :after ((w general-buttons-window) &key)
  "Creates the window and its widgets"
  (let*-slots w
      ((window (ag:window-new :noborders))
       (show-last-trick-btn
	(ag:new-button window nil
		       "Letzten Stich zeigen"
		       (std-event-handler
			 (show-last-trick (module w)))))
       (leave-btn (ag:new-button
		   window nil "Die Runde verlassen"
		   (std-event-handler
		     (leave-requested (module w))))))
    (ag:disable-widget show-last-trick-btn)
    (ag:set-window-position window :br nil)))

(defmethod trick-available ((w general-buttons))
  (with-slots (show-last-trick-btn) w
    (ag:enable-widget show-last-trick-btn)))

(defclass general-buttons (module)
  ((window :accessor window)))

(defmethod cleanup ((module general-buttons))
  (hide (window module))
  (get-rid-of-window (window (window module)))
  (slot-makunbound module 'window))

(defmethod initialize-instance :after ((module general-buttons) &key)
  "Creates the genral buttons window"
  (setf (window module) (make-instance 'general-buttons-window :module module)))

(defmethod show-last-trick ((module general-buttons))
  "Requests the UI to show the last trick"
  (show-last-trick (ui module)))

(defmethod leave ((module general-buttons))
  "Makes the UI leave. And hides the general-buttons window"
  (hide (window module))
  (leave (ui module)))

(defmethod leave-requested ((module general-buttons))
  "Asks with a dialog whether the player is sure"
  (ag:prompt-options "Willst du die Runde wirklich verlassen?"
		     (list "Ja"
			   (std-event-handler
			     (leave module)))
		     (list "Nein"
			   (std-event-handler ; idle
			     ))))

(defmethod game-starts ((module general-buttons))
  "Show the general-buttons window"
  (show (window module)))

(defmethod trick-available ((module general-buttons))
  "Enables the show-last-trick button"
  (trick-available (window module)))

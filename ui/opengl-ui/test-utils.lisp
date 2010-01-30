(in-package skat-opengl-ui)

;; Hier sollen Makros rein, die das Testen von Modulen usw. erleichtern.

(defmacro with-skat-window (&body body)
  "Initialisiert SDL und erstellt das Skatfenster."
  `(sdl:with-init ()
     (skat-window)
     ,@body))

(defmacro standard-with-events (&body event-handlers)
  "Definiert das Quit-Event und startet die Schleife"
  `(sdl:with-events (:poll sdl-event)
     (:quit-event () t)
     ,@event-handlers))

(defmacro with-agar-enabled-skat-window (&body body)
  "Initialisiert SDL, das Skatfenster und Agar"
  `(with-skat-window
     (agar:with-agar-core ("Skat")
       (agar:with-sdl-video ((sdl:fp sdl:*default-display*) :opengl) ; AG initialisieren und anbinden
	 ,@body))))

(defmacro test-module (module-class &rest initargs)
  (let ((module (gensym "module")))
    `(with-agar-enabled-skat-window
       (let ((,module (make-instance ,module-class ,@initargs)))
	 (standard-with-events
	   ((:mouse-button-down-event ()
				      (handle-event ,module sdl-event))
	    (:mouse-button-up-event ()
				    (handle-event ,module sdl-event))
	    (:idle ()
		   (agar:render
		     (draw ,module)))))))))

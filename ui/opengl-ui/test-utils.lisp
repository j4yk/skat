(in-package skat-opengl-ui)

;; Hier sollen Makros rein, die das Testen von Modulen usw. erleichtern.

(defmacro with-skat-window (&body body)
  "Initialisiert SDL und erstellt das Skatfenster."
  `(sdl:with-init ()
     (skat-window)
     (init-gl 640 480)
     ,@body))

(defmacro standard-with-events (&body event-handlers)
  "Definiert das Quit-Event und startet die Schleife"
  `(sdl:with-events (:poll sdl-event)
     (:quit-event () t)
     ,@event-handlers))

(defmacro with-agar-enabled-skat-window (&body body)
  "Initialisiert SDL, das Skatfenster und Agar"
  `(with-skat-window
     #+agar
     (agar:with-agar-core ("Skat")
       (agar:with-sdl-video ((sdl:fp sdl:*default-display*) :opengl) ; AG initialisieren und anbinden
	 ,@body))
     #-agar
     (progn
       ,@body)))

(defmacro test-module (module-class &rest initargs)
  (assert (find-class (cadr module-class))) ; quoted!
  (let ((module (gensym "module")))
    `(with-agar-enabled-skat-window
       (sdl:show-cursor :enable)
       (let ((,module (make-instance ,module-class ,@initargs))
	     (ui (make-instance 'opengl-ui)))
	 (push ,module (modules ui))
	 (standard-main-loop ui)))))

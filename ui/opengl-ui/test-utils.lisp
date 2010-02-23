(in-package skat-opengl-ui)

;; Hier sollen Makros rein, die das Testen von Modulen usw. erleichtern.

;; (defmacro with-skat-window (&body body)
;;   "Initialisiert SDL und erstellt das Skatfenster."
;;   `(sdl:with-init (sdl:sdl-init-video)
;;      (skat-window)
;;      (init-gl 640 480)
;;      ,@body))

;; (defmacro standard-with-events (&body event-handlers)
;;   "Definiert das Quit-Event und startet die Schleife"
;;   `(sdl:with-events (:poll sdl-event)
;;      (:quit-event () t)
;;      ,@event-handlers))

;; (defmacro with-agar-enabled-skat-window (&body body)
;;   "Initialisiert SDL, das Skatfenster und Agar"
;;   `(with-skat-window
;;      #+agar
;;      (agar:with-agar-core ("Skat")
;;        (agar:with-sdl-video ((sdl:fp sdl:*default-display*) :opengl :overlay) ; AG initialisieren und anbinden
;; 	 ,@body))
;;      #-agar
;;      (progn
;;        ,@body)))

(defun init-window (&optional sdl-flags)
  (sdl:init-sdl :flags sdl-flags :video t)
  #+windows (sdl:init-image :png)
  (defparameter *sdl-screen* (skat-window)))

(defun init-agar (&rest flags)
  #+agar
  (apply #'ag::init-video-sdl (sdl:fp *sdl-screen*) flags))

(defun kill-video ()
  #+agar
  (ag::destroy-video)
  (sdl:quit-sdl)
  (defparameter *sdl-screen* nil)
  (values))

(defun setup-video ()
  (init-window)
  (init-agar))

(defun test-module (module-class &rest initargs)
  (assert (find-class module-class))
  (defparameter *module* (apply #'make-instance module-class initargs))
  (unless (boundp 'ui)
    (defparameter ui (make-instance 'opengl-ui))
    (push (make-instance 'agar) (modules ui)))
  (unless (member (find-class module-class) (modules ui) :key #'class-of)
    (push *module* (modules ui)))
  (standard-main-loop ui))

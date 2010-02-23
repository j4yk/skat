(in-package skat-opengl-ui)

;; Hier sollen die Sachen rein, die das Testen von Modulen usw. erleichtern.

(defparameter *sdl-screen* nil)

(defun init-window (&rest sdl-flags)
  (sdl:init-sdl :flags sdl-flags :video t)
  #+windows (sdl:init-image :png)
  (defparameter *sdl-screen* (skat-window)))

(defun init-agar (&rest flags)
  (assert *sdl-screen*)
  #+agar
  (ag::init-core "skat" 0)
  #+agar
  (apply #'ag::init-video-sdl (sdl:fp *sdl-screen*) flags))

(defun kill-video ()
  #+agar
  (ag::destroy-video)
  (sdl:close-audio)
  (sdl:quit-sdl)
  (setq *sdl-screen* nil)
  (values))

(defun setup-video ()
  (init-window)
  (init-agar :overlay))

(defparameter *ui* nil)
(defparameter *module* nil)

(defun test-module (module-class &rest initargs)
  (assert (find-class module-class))
  (setq *module* (apply #'make-instance module-class initargs))
  (unless *ui*
    (setq *ui* (make-instance 'opengl-ui))
    (push (make-instance 'agar) (modules *ui*)))
  (unless (member (find-class module-class) (modules *ui*) :key #'class-of)
    (push *module* (modules *ui*)))
  (standard-main-loop *ui*))

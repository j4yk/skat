(in-package skat-opengl-ui)

;; Hier sollen die Sachen rein, die das Testen von Modulen usw. erleichtern.

(defparameter *sdl-screen* nil)

(defun init-window (&rest sdl-flags)
  (sdl:init-sdl :flags sdl-flags :video t)
  #+windows (sdl:init-image :png)
  (defparameter *sdl-screen* (skat-window)))

(defun init-agar (&rest flags)
  (assert *sdl-screen*)
  (ag::init-core "skat" 0)
  (apply #'ag::init-video-sdl (sdl:fp *sdl-screen*) flags))

(defun kill-video ()
  (ag:destroy-video)
  (sdl:close-audio)
  (sdl:quit-sdl)
  (setq *sdl-screen* nil)
  (values))

(defun setup-video ()
  (init-window)
  (init-agar))

(defun clean-agar ()
  (ag:destroy-video)
  (init-agar))

(defparameter *ui* nil)
(defparameter *module* nil)

(defun test-module (module-class &rest initargs)
  (assert (find-class module-class))
  (unless *ui*
    (setq *ui* (make-instance 'opengl-ui))
    (push (make-instance 'agar :ui *ui*) (modules *ui*)))
  (unless (find-module module-class *ui*)
    (let ((module (apply #'make-instance module-class :ui *ui* initargs)))
      (push module (modules *ui*))))
  (standard-main-loop *ui*))

(defun reset-module (module-class &rest initargs)
  (when (find-module module-class *ui*)
    (cleanup-and-remove-module (find-module module-class *ui*) *ui*))
  (apply #'test-module module-class initargs))

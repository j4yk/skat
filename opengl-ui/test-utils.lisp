(in-package skat-opengl-ui)

;; Hier sollen die Sachen rein, die das Testen von Modulen usw. erleichtern.

(defvar *sdl-screen* nil)

(defun get-some-cards (&optional (n 10))
  (flet ((new-card ()
	   (kern::make-card :suit (nth (random 4) (mapcar #'car kern::*card-suits*))
			    :rank (nth (random 8) (mapcar #'car kern::*card-ranks*)))))
    (loop with cards = nil
       do (setf cards (delete-duplicates (cons (new-card) cards) :test #'equalp))
       until (= n (length cards))
       finally (return cards))))

(defun init-window (&rest sdl-flags)
  (sdl:init-sdl :flags sdl-flags :video t)
  #+windows (sdl:init-image :png)
  (defparameter *sdl-screen* (skat-window)))

(defun init-agar (&rest flags)
  (assert *sdl-screen*)
  (ag::init-core "skat" 0)
  (apply #'ag::init-video-sdl (sdl:fp *sdl-screen*) flags))

(defun kill-video ()
  (sdl:push-quit-event)
  (sleep 1) ; give it time to finish
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

(defvar *ui* nil)

(defun setup-ui ()
  (unless *ui*
    (setq *ui* (make-instance 'opengl-ui))
    (start *UI*)
    (unless (find-module 'agar *ui*)
      (insert-module (make-instance 'agar :ui *ui*) *ui*)))
  *ui*)

(defun test-module (module-class &rest initargs)
  (assert (find-class module-class))
  (setup-ui)
  (unless (find-module module-class *ui*)
    (let ((module (apply #'make-instance module-class :ui *ui* initargs)))
      (push module (modules *ui*))))
  (standard-main-loop *ui*))

(defun reset-module (module-class &rest initargs)
  (let ((m (find-module module-class *ui*)))
    (when m
      (cleanup m)
      (apply #'initialize-instance m initargs))))

(defmacro fmod (module-class)
  `(find-module ',module-class *ui*))

(defun tstat ()
  "timeout status"
  (loop for to in (list :trick-to :trick-push :choose-card :middle-stack-push)
     appending (list to (timeout-scheduled-p (fmod cards) to))))

(defun msp ()
  "push some cards"
  (mapcar (curry 'middle-stack-push (fmod cards)) (get-some-cards 3) (list :left :right :self))
  (tstat))

(defun trick-away ()
  "push trick away"
  (trick-to (fmod cards) :self)
  (tstat))

(defun cs ()
  "choose-card"
  (choose-card (fmod cards))
  (tstat))

(defun delto (to)
  "delete timeout by keyword"
  (ag:delete-timeout (null-pointer) (getf (timeouts (fmod cards)) to))
  (setf (queued-args (fmod cards) to) nil)
  (tstat))

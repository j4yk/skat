(defpackage skat-opengl-ui
  (:nicknames opengl-ui gl-ui)
  (:use :cl :skat-ui))

(in-package gl-ui)

(defmacro restartable (&body body)
  `(restart-case (progn ,@body)
     (continue () t)))

(defclass opengl-ui (ui::base-ui)
  ()
  (:documentation "OpenGL-UI Klasse. Zentrales Objekt für die OpenGL-Schnittstelle"))

(defmethod ui:start ((ui opengl-ui) &optional no-new-thread-p)
  "Startet die OpenGL-Benutzerschnittstelle.
STUB"
  (declare (ignore ui no-new-thread-p))
  (error "Not implemented yet!"))

(defmethod ui:stop ((ui opengl-ui))
  "Hält die OpenGL-Benutzerschnittstelle an.
STUB"
  (declare (ignore ui))
  (error "Not implemented yet!"))

(defvar *textures-updated* nil)

(defmethod main-gl-init (ui)
  (declare (ignore ui))
  (setq *textures-updated* nil)
  (gl:clear-color 0 1 0 0)
  ;; Perspektive
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:perspective 60.0 1.0 0.1 1024.0)
  (gl:matrix-mode :modelview))

(defvar *blue-tex*)
(defvar *texture*)

(defmethod onidle ()
  (update-textures)			; wenn es neues in dieser Funktion gibt, führe das aus
  (gl:enable :texture-2d)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gl:load-identity)
  (gl:translate 0 0 -10)
  (gl:bind-texture :texture-2d *blue-tex*)
  (gl:matrix-mode :texture)
  (gl:load-identity)			; wahrscheinlich braucht man das gar nicht immer
  (gl:matrix-mode :modelview)
  ;; Blaues Dreieck
  (gl:color 1 1 1)
  (gl:with-primitive :triangles
    (gl:tex-coord 0 1) (gl:vertex 0 2 -1)
    (gl:tex-coord 0 0) (gl:vertex 0 0 -1)
    (gl:tex-coord 1 0) (gl:vertex 1 0 -2))
  
  (gl:bind-texture :texture-2d *texture*)
  (gl:matrix-mode :texture)
  (gl:load-identity)			; siehe oben
  (gl:matrix-mode :modelview)
  ;; verzerrte Testkarte
  (gl:color 1 1 1)
  (gl:with-primitive :quads
    (gl:tex-coord 0 1) (gl:vertex -3 -3)
    (gl:tex-coord 1 1) (gl:vertex 5 -3)
    (gl:tex-coord 1 0) (gl:vertex 5 0)
    (gl:tex-coord 0 0) (gl:vertex -3 0))

  ;; zeigen
  (gl:flush)
  (sdl:update-display)
  (gl:disable :texture-2d))		; braucht man bestimmt auch nicht

(defmethod sdl-surface-to-gl-texture (surface &optional (texture-format :rgba))
  "Erstellt eine OpenGL-Textur aus einer SDL-Surface. Gibt die Textur-ID zurück."
  (let ((texture (car (gl:gen-textures 1)))) ; Textur generieren
    (gl:bind-texture :texture-2d texture)
    ;; Filteroptionen für die Textur einstellen
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
    ;; die Pixel aus der SDL-Surface kopieren
    (sdl-base::with-pixel (pixels (sdl:fp surface))
      (gl:tex-image-2d :texture-2d 0 :rgba ;(sdl-base::pixel-bpp pixels)
		       (sdl:width surface) (sdl:height surface)
		       0 texture-format :unsigned-byte (sdl-base::pixel-data pixels)))
    ;; und die Texturnummer zurückgeben
    texture))

(defun ensure-file-exists (filename)
  (unless (probe-file filename)
    (error "File ~a doesn't exist!" filename))
  filename)

(defun texture-from-bmp (filename)
  "Erstellt eine OpenGL-Textur aus einer BMP-Datei.
Gibt die Textur-ID zurück."
  (declare (ftype (function (string) integer) texture-from-bmp))
  (sdl:with-surface (s (sdl:load-image (ensure-file-exists filename))) ; Bild auf ein SDL-Surface laden
    (sdl-surface-to-gl-texture s :bgra)))		       ; in OpenGL-Textur umwandeln

(progn
  (eval-when (:compile-toplevel)
    (setq *textures-updated* nil))
  (defmethod update-textures ()
    (unless *textures-updated*
      (setq *textures-updated* t)
      (format t "~&Updating textures~%")
      (macrolet ((conditional-delete (var)
		   `(if (boundp ',var)
			(gl:delete-textures (list ,var)))))
	(conditional-delete *texture*)
	(conditional-delete *blue-tex*))
      ;; eine per Hand erstellte Textur... blaue Fläche
      (setq *blue-tex* (sdl:with-surface (s (sdl:create-surface 512 512))
			 (sdl:fill-surface-* 0 0 255 :clipping nil)
			 ;; mit clipping gibts eine Exception wegen Null-Pointer, 
			 ;; da der hier nicht 
			 (sdl-surface-to-gl-texture s :rgba)))
      ;; und eine aus dem Bild
      (setq *texture* (texture-from-bmp "diamonds7.bmp")))))

(defun skat-window ()
  "Erstellt das Skat-SDL-Fenster inklusive OpenGL-Kontext."
  (sdl:window 640 480 :title-caption "Skat"
	      :flags '(sdl:sdl-opengl sdl:sdl-doublebuf)))

(defun sdl-main-loop (ui)
  (sdl:with-init ()
    (skat-window)
    (setf (sdl:frame-rate) 2)
    (main-gl-init ui)
    (sdl:show-cursor :enable)		; mit Cursor bitte
;    (sdl-ttf:init-ttf)
;    (setf sdl:*default-font* (sdl-ttf:open-font (make-instance 'sdl:ttf-font-definition :filename "/usr/share/fonts/truetype/freefont/FreeSans.ttf" :size 12)))
    (update-textures)
    (assert sdl:*default-font*)
    (sdl:with-events ()
      (:quit-event ()
;		   (sdl-ttf:close-font :font sdl:*default-font*)
;		   (sdl-ttf:quit-ttf)
		   (gl:delete-textures (list *texture* *blue-tex*))
		   (mapcar #'makunbound '(*texture* *blue-tex*))
		   t)
      (:idle ()
	     (restartable			; SLIME-Sachen ausführen
	       (let ((connection
		      (or swank::*emacs-connection* (swank::default-connection))))
		 (when (and connection (not (eql swank:*communication-style* :spawn)))
		   (swank::handle-requests connection t))))
	     (onidle)))))		
  
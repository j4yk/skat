(in-package skat-opengl-ui)

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

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

(defun create-solid-filled-texture (r g b &optional (a 1))
  (sdl:with-surface (s (sdl:create-surface 32 32))
    (sdl:fill-surface-* (round (* 255 r))
			(round (* 255 g))
			(round (* 255 b))
			:a (* 255 a) :clipping nil)
    ;; mit clippingn gibts eine Exception wegen Null-Pointer, 
    ;; da der hier nicht 
    (sdl-surface-to-gl-texture s :rgba)))

;; (defmacro selection (x y &body body)
;;   `(with-foreign-object (buffer :uint 256)
;;      (let ((viewport (get-integer :viewport)))
;;        (unwind-protect
;;             (progn
;;               (%gl:select-buffer 256 buffer)
;;               (matrix-mode :projection)
;;               (render-mode :select)
;;               (with-pushed-matrix
;;                 (load-identity)
;;                 (glu:pick-matrix ,x (- (aref viewport 3) ,y) 1.0 1.0 viewport)
;;                 (glu:perspective 60.0 (/ (aref viewport 2) (aref viewport 3)) 1 256)
;;                 ,@body
;;                 (matrix-mode :projection))
;;               (let ((treffer (render-mode :render)))
;;                 (loop with getroffen = (- (expt 2 32) 1)
;;                    with z-wert = (- (expt 2 32) 1)
;;                    for i from 0 to (1- treffer)
;;                    do (if (< (mem-aref buffer :uint (+ (* i 4) 1)) z-wert)
;;                           (setf getroffen (mem-aref buffer :uint (+ (* i 4) 3))
;;                                 z-wert (mem-aref buffer :uint (+ (* i 4) 1))))
;;                    finally (return (if (= getroffen (- (expt 2 32) 1))
;;                                        -1
;;                                        getroffen)))))))))

(defpackage skat-opengl-ui
  (:nicknames opengl-ui gl-ui)
  (:use :cl :skat-ui :skat-utils :cffi)
  (:shadow #:callback)
  (:shadowing-import-from cffi defcallback null-pointer pointer-eq foreign-alloc foreign-free lisp-string-to-foreign))

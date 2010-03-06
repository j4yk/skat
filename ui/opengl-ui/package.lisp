(defpackage skat-opengl-ui
  (:nicknames opengl-ui gl-ui)
  (:shadowing-import-from cffi defcallback null-pointer pointer-eq foreign-alloc foreign-free lisp-string-to-foreign)
  (:use :cl :skat-ui :skat-utils))

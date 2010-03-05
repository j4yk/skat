(defpackage skat-opengl-ui
  (:nicknames opengl-ui gl-ui)
  (:shadowing-import-from cffi defcallback null-pointer pointer-eq)
  (:use :cl :skat-ui :skat-utils))

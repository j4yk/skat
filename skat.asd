(defpackage skat-system
  (:use :cl :asdf))

(in-package skat-system)

(defsystem skat
  :description "Skatspiel über XMPP"
  :version "0.1"
  :author "Jakob Reschke <jakob@resfarm.de>"
  :license "GNU General Public License"
  :depends-on (:cl-xmpp-tls :cl-glu :cl-glut :lispbuilder-sdl)
  :components ((:file skat-packages)
	       (:module comm
			:serial t
			:components ((:file base-comm)
				     (:file xmpp-comm)))
	       (:module ui
			:serial t
			:depends-on (skat-packages)
			:components ((:file base-ui)
				     (:module opengl-ui)))
	       (:module kernel
			:depends-on (skat-packages)
			:serial t
			:components ((:file host)
				     (:file player)))))

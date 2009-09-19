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
	       (:file requests)
	       (:module comm
			:serial t
			:depends-on (skat-packages)
			:components ((:file base-comm)
				     (:file xmpp-comm)))
	       (:module ui
			:serial t
			:depends-on (skat-packages)
			:components ((:file base-ui)
				     (:module opengl-ui)))
	       (:module kernel
			:depends-on (skat-packages requests)
			:serial t
			:components ((:file cards)
				     (:file request-handling)
				     (:file base-kernel)
				     (:file host)
				     (:file player)))))

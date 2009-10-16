(defpackage skat-system
  (:use :cl :asdf))

(in-package skat-system)

(defsystem skat
  :description "Skatspiel über XMPP"
  :version "0.1"
  :author "Jakob Reschke <jakob@resfarm.de>"
  :license "GNU General Public License"
  :depends-on (:cl-xmpp-tls :cl-glu :cl-glut :lispbuilder-sdl)
  :serial t
  :components ((:file skat-packages)
	       (:file utils)
	       (:file requests)
	       (:module comm
			:serial t
			:components ((:file base-comm)
				     (:file xmpp-comm)))
	       (:module ui
			:serial t
			:components ((:file base-ui)
				     (:module opengl-ui)))
	       (:module kernel
			:serial t
			:components ((:file cards)
				     (:file tricks)
				     (:file bidding)
				     (:file request-handling)
				     (:file ringlist)
				     (:file kernel)
				     (:file states :depends-on (kernel))
				     (:file host)
				     (:file player)))))

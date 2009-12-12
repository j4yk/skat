(defpackage skat-system
  (:use :cl :asdf))

(in-package skat-system)

(defsystem skat
  :description "Skatspiel über XMPP"
  :version "0.1"
  :author "Jakob Reschke <jakob@resfarm.de>"
  :license "GNU General Public License"
  :depends-on (:cl-xmpp-tls :cl-glu :cl-glut :lispbuilder-sdl :clunit)
  :serial t
  :components ((:file skat-packages)
	       (:file utils)
	       (:file requests)
	       (:file request-handling)
	       (:file login-and-registration)
	       (:file struct-translations)
	       (:module comm
			:serial t
			:components ((:file base-comm)
				     (:file stub-comm)
				     (:file xmpp-comm)))
	       (:module ui
			:depends-on (request-handling)
			:serial t
			:components ((:file ui-request-handling)
				     (:file base-ui)
				     (:file stub-ui)
				     (:file host-ui)
				     (:module opengl-ui)))
	       (:module kernel
			:serial t
			:components ((:file cards)
				     (:file tricks)
				     (:file bidding)
				     (:file scoring)
				     (:file ringlist)
				     (:file kernel)
				     (:file states :depends-on (kernel))
				     (:file host)
				     (:file host-tests)
				     (:file player)
				     (:file player-tests)))
	       (:file debugutils)))

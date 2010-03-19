(defpackage skat-system
  (:use :cl :asdf))

(in-package skat-system)

(defsystem skat
  :description "Skatspiel über XMPP"
  :version "0.1"
  :author "Jakob Reschke <jakob@resfarm.de>"
  :license "GNU General Public License"
  :depends-on (:cl-xmpp-tls #+clunit :clunit)
  :serial t
  :components (;(:file skat-packages)
	       (:file utils)
	       (:file requests)
	       (:file modules)		; skat-kernel, skat-ui, skat-comm
	       (:file request-handling :depends-on (modules))
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
				     (:file host :depends-on (kernel states))
				     (:file player :depends-on (kernel states cards))
				     #+clunit (:file player-tests :depends-on (host player)))
	       (:file debugutils)))

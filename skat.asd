(defpackage skat-systems
  (:use :cl :asdf))

(in-package skat-systems)

(defsystem skat-core
  :version "0.1"
  :author "Jakob Reschke <jakob@resfarm.de>"
  :license "GNU General Public License"
  :depends-on (#+clunit :clunit)
  :components ((:file hacks)
	       (:file utils :depends-on (hacks))
	       (:file requests :depends-on (utils))
	       (:file components :depends-on (requests))
	       (:file request-handling :depends-on (components))
	       (:file login-and-registration :depends-on (components))
	       (:file struct-translations :depends-on (components))
	       (:module comm
			:depends-on (components login-and-registration)
			:components ((:file base-comm)
				     (:file stub-comm :depends-on (base-comm))))
	       (:module ui
			:depends-on (components)
			:components ((:file ui-request-handling)
				     (:file base-ui :depends-on (ui-request-handling))
				     (:file stub-ui :depends-on (base-ui))
				     (:file host-ui :depends-on (base-ui))))
	       (:module kernel
			:depends-on (comm request-handling login-and-registration)
			:components ((:file cards)
				     (:file tricks :depends-on (cards))
				     (:file bidding)
				     (:file scoring :depends-on (cards))
				     (:file ringlist)
				     (:file kernel)
				     (:file states :depends-on (kernel))
				     (:file host :depends-on (kernel states))
				     (:file player :depends-on (kernel states cards))
				     #+clunit (:file player-tests :depends-on (host player))))))

(defsystem skat-xmpp-comm
  :author "Jakob Reschke <jakob@resfarm.de>"
  :license "GNU General Public License"
  :depends-on (skat-core :cl-xmpp-tls)
  :components ((:module comm
			:components ((:file xmpp-comm)))))

(defsystem skat-opengl-ui
  :author "Jakob Reschke <jakob@resfarm.de>"
  :license "GNU General Public License"
  :depends-on (skat-core skat-xmpp-comm	; objective should be to remove the xmpp dependency
	       :cffi :trivial-garbage
	       :lispbuilder-sdl :lispbuilder-sdl-image :cl-opengl :cl-glu #+agar :agar)
  :components ((:module ui
			:components ((:module opengl-ui
					      :components ((:file package)
							   (:file module :depends-on (package))
							   (:file gfx-utils :depends-on (package))
							   (:file agar :depends-on (module))
							   (:file gl-ui :depends-on (module gfx-utils agar))
							   (:file test-module :depends-on (gl-ui))
							   #+agar (:file agar-test :depends-on (gl-ui agar))
							   #+agar (:file login-and-register :depends-on (gl-ui))
							   (:file test-utils :depends-on (gl-ui))
							   (:file selection :depends-on (gfx-utils gl-ui))
							   (:file selection-test :depends-on (selection module gl-ui))
							   (:file cards :depends-on (selection gl-ui))))))))

(defsystem skat
  :description "Skat with an OpenGL UI via XMPP"
  :author "Jakob Reschke <jakob@resfarm.de>"
  :license "GNU General Public License"
  :depends-on (skat-core skat-xmpp-comm skat-opengl-ui)
  :components ((:file debugutils)))

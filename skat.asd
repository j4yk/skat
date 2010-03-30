(defpackage skat-systems
  (:use :cl :asdf))

(in-package skat-systems)

(defsystem skat
  :version "0.1"
  :author "Jakob Reschke <jakob@resfarm.de>"
  :license "GNU General Public License"
  :depends-on (cffi
	       trivial-garbage trivial-timeout bordeaux-threads
	       cl-xmpp-tls
	       lispbuilder-sdl lispbuilder-sdl-image cl-opengl cl-glu agar
	       #+clunit clunit)
  :components ((:file hacks)
	       (:file utils :depends-on (hacks))
	       (:file requests :depends-on (utils))
	       (:file components :depends-on (requests))
	       (:file request-handling :depends-on (components))
	       (:file login-and-registration :depends-on (components))
	       (:file struct-translations :depends-on (components))
	       ;; abstract components
	       (:module comm
			:depends-on (components login-and-registration)
			:components ((:file base-comm)
				     (:file stub-comm :depends-on (base-comm))))
	       (:module ui
			:depends-on (components request-handling)
			:components ((:file ui-request-handling)
				     (:file base-ui :depends-on (ui-request-handling))
				     (:file stub-ui :depends-on (base-ui))))
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
				     (:file player :depends-on (kernel states cards))))
	       #+clunit (:module tests
				 :depends-on (kernel comm ui host-ui)
				 :components ((:file player-tests)))
	       ;; starting mechanism
	       (:module start :depends-on (comm ui)
			:components ((:file start)))
	       ;; implemented components
	       (:module host-ui :depends-on (ui)
			:components ((:file host-ui)))
	       (:module xmpp-comm
			:depends-on (comm)
			:components ((:file xmpp-comm)))
	       (:module opengl-ui
			:depends-on (ui xmpp-comm kernel)
			:components ((:file package)
				     (:file test-functions :depends-on (package))
				     (:file module :depends-on (package))
				     (:file gfx-utils :depends-on (package))
				     (:file gl-ui-utils :depends-on (gfx-utils))
				     (:file selection :depends-on (gfx-utils gl-ui-utils))
				     (:file selection-test :depends-on (selection module))
				     (:file agar :depends-on (module))
				     (:file error-handling :depends-on (module agar))
				     (:file login-and-register :depends-on (module agar))
				     (:file bidding :depends-on (module agar))
				     (:file declaration :depends-on (module agar))
				     (:file players :depends-on (module agar declaration))
				     (:file after-game :depends-on (module agar declaration))
				     (:file cards :depends-on (gfx-utils selection
									 gl-ui-utils agar))
				     (:file general-buttons :depends-on (module agar))
				     (:file gl-ui
					    :depends-on (module
							 gfx-utils
							 agar
							 error-handling
							 gl-ui-utils
							 cards
							 login-and-register
							 players
							 declaration
							 general-buttons
							 after-game))
				     (:file test-utils :depends-on (gl-ui))
				     (:file agar-test :depends-on (gl-ui agar))
				     (:file test-module :depends-on (gl-ui))))
	       (:file debugutils :depends-on (kernel comm ui host-ui opengl-ui))
	       (:file test-utils :depends-on (xmpp-comm))
	       (:file toplevel :depends-on (start xmpp-comm opengl-ui))))

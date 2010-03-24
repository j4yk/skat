all: skat-player skat-host

skat-image:
	sbcl --eval "(progn \
                        (require 'skat) \
                        (save-lisp-and-die \"skat-image\"))"

skat-player: skat-image
	sbcl --core skat-image \
             --eval "(funcall (intern (symbol-name '#:create-gl-ui-xmpp-player-executable) :kern) \
                      	      \"skat-player\")"

skat-host: skat-image
	sbcl --core skat-image \
             --eval "(funcall (intern (symbol-name '#:create-host-xmpp-executable) :kern) \
                              \"skat-host\")"

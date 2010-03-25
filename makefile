SHARED_ROOT=$(DESTDIR)/usr/share/skat
BIN=$(DESTDIR)/usr/bin

all: skat-player skat-host

skat-image:
	sbcl --eval "(progn \
                        (require 'skat) \
                        (save-lisp-and-die \"skat-image\"))"

skat-player: skat-image
	sbcl --core skat-image \
             --eval "(kern::create-gl-ui-xmpp-player-executable \"skat-player\")"

skat-host: skat-image
	sbcl --core skat-image \
             --eval "(kern::create-host-xmpp-executable \"skat-host\")"

install: skat-player skat-host resources
	install -d $(BIN) $(SHARED_ROOT)/cards
	install resources/cards/* -t $(SHARED_ROOT)/cards
	install skat-player $(BIN)/skat-player
	install skat-host $(BIN)/skat-host

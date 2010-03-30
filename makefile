SHARED_ROOT=$(DESTDIR)/usr/share/skat
BIN=$(DESTDIR)/usr/bin

all: skat.core

dependencies.core: dependencies.lisp
	sbcl --script dependencies.lisp

skat.core: dependencies.core
	sbcl --core dependencies.core --script make-image.lisp

install: skat.core skat-player skat-host resources
	install -d $(BIN) $(SHARED_ROOT)/cards
	install resources/cards/* -t $(SHARED_ROOT)/cards
	install skat.core $(BIN)/skat.core
	install skat-player $(BIN)/skat-player
	install skat-host $(BIN)/skat-host

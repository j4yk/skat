;; Hier werden die Pakete aller Modulklassen definiert

(in-package skat-utils)

(define-package-which-exports-all-requests skat-kernel
  (:nicknames :kernel :kern)
  (:use cl skat-utils org.ancar.clunit)
  (:export host
	   player
	   handler-fn-name
	   handler-fn
	   receive-requests
	   call-handler-fn
	   error-in-handler
	   define-login-data	        ; Login- Strutkturen für Comm
	   define-registration-data	; Registrierungs-Strutkturen für Comm
	   slots-of-class		; Slotliste holen
	   make-card
	   print-card
	   sort-cards
	   suit
	   rank
	   *game-point-levels*
	   cut-away-game-point-levels))
  

(define-package-which-exports-all-requests skat-ui
  (:nicknames :ui)
  (:use cl skat-utils)
  (:export handler-fn-name
	   handler-fn
	   call-handler-fn
	   start
	   stop
	   stub-ui
	   host-ui
	   just-one-step
	   defhandler
	   call-kernel-handler
	   sender
	   ui				; manche UIs haben eigene Packages, defhandler benutzt aber dieses Symbol
	   kernel))			; gleiches Problem, UIs mit eigenen Packages müssen an den Kernel rankommen

(define-package-which-exports-all-requests skat-communication
  (:nicknames :skat-comm :comm)
  (:use cl skat-utils)
  (:export start ; startet das Kommunikationsobjekt
	   login ; lässt es sich einwählen
	   register ; weist zur Registrierung mit einem Host an
	   get-request ; holt eine Anfrage aus ihm heraus
	   has-request ; gibt darüber Auskunft, ob Anfragen vorliegen
	   send  ; beauftragt es, eine Anfrage zu verschicken
	   stop  ; gibt ihm das Signal, die Arbeit einzustellen
	   received-other-content ; Condition für den Fall, dass Daten empfangen wurden, die nicht als Anfragen interpretiert werden konnten
	   address-compare-function ; Slot-Accessor für die Adressenvergleichende Funktion
	   stub-comm
	   xmpp-comm))

(defmacro define-skat-packages ((&rest requests-kernel-ui) (&rest requests-kernel-comm))
  `(progn
     (defpackage skat-kernel
       (:nicknames :kernel :kern)
       (:use cl skat-utils org.ancar.clunit)
       (:export handler-fn-name
		handler-fn
		receive-requests
		call-handler-fn
		error-in-handler
		define-login-data	 ; Login-Strukturen für Comm
		define-registration-data ; Registrierungs-Strukturen für Comm
		slots-of-class		 ; Slotliste holen
		,@requests-kernel-ui
		,@requests-kernel-comm))
     (defpackage skat-ui
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
		,@requests-kernel-ui))
     (defpackage skat-communication
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
		xmpp-comm
		,@requests-kernel-comm))))


(define-skat-packages 
    (login-struct
     registration-struct
     registration-reply
     server-update
     playmates
     game-start
     cards
     start-bidding
     listen
     bid
     join
     pass
     declarer
     hand-decision
     skat
     declaration
     choose-card
     trick
     game-over
     cards-score
     game-result
     match-score
     game-end
     logout) 
    ())

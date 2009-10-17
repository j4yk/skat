(defpackage skat-utils
  (:nicknames :utils)
  (:use :cl :org.ancar.clunit)
  (:export to-keyword
	   parse-function-body
	   separate-lambda-list
	   deftestfn))

(defpackage skat-requests
  (:nicknames requests)
  (:use cl skat-utils)
  (:export correct-parameters-p
	   request-parameters))

(defmacro define-skat-packages ((&rest requests-kernel-ui) (&rest requests-kernel-comm))
  `(progn
     (defpackage skat-kernel
       (:nicknames :kernel :kern)
       (:use cl skat-utils org.ancar.clunit)
       (:export ,@requests-kernel-ui
		,@requests-kernel-comm))
     (defpackage skat-ui
       (:nicknames :ui)
       (:use cl skat-utils)
       (:export ,@requests-kernel-ui))
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
		,@requests-kernel-comm))))


(define-skat-packages 
    (login-parameters
     registration-parameters
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

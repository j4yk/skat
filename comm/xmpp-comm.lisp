(in-package skat-comm)

(defclass xmpp-skat-connection (xmpp:connection)
  ((comm-object :accessor comm-object)))

(defclass xmpp-comm (base-comm)
  ((connection :accessor connection)
   (stop-working :accessor stop-working :initform nil)
   (resource :accessor resource)
   (address :accessor address)
   (address-compare-function :initform #'equalp))
  (:documentation "Communication over an XMPP-Connection. The addresses are JID-strings."))

(defmethod start ((comm xmpp-comm))
  "Initialisiert dieses XMPP-Verbindungsobjekt. Fügt nur die Login-Parameterliste für Kernel zum Abruf ein."
  (push-request comm comm 'login-parameters 
		(list '((username string "XMPP Benutzername") 
			(hostname string "Adresse des XMPP Servers") 
			(jid-domain-part string "Domänen-Teil der JID (z. B. \"bar.org\" in \"foo@bar.org\"), falls verschieden von der Serveradresse") 
			(resource string "XMPP-Ressource zum Einloggen")
			(password string "Passwort")
			(mechanism (:plain :sasl-plain) "Form, in der das Passwort übermittelt wird"))))
  (values))

(defun make-xmpp-login-data (username hostname domain resource password mechanism)
  "Erstellt eine Login-Data-Liste für xmpp-comm"
  (apply #'append (mapcar #'list
			  '(:username :hostname :jid-domain-part
			    :resource :password :mechanism)
			  (list username hostname domain
				resource password mechanism))))

(defmacro let-multiple-getf (place (&rest indicators-and-varnames) &body body)
  "Packt eine Property-Liste mit getf aus.
Syntax: let-multiple-getf place ({indicator varname}*) form*"
  (let ((indicators (loop for i from 0 to (1- (length indicators-and-varnames))
			 when (evenp i) collect (nth i indicators-and-varnames))))
    `(let ,(loop for indicator in indicators
		collect `(,(getf indicators-and-varnames indicator) (getf ,place ,indicator)))
       ,@body)))

(define-condition login-unsuccessful (error)
  ((additional-information :accessor additional-information :initarg :additional-information)))

(defmethod login ((comm xmpp-comm) data)
  "Stellt die XMPP-Verbindung zum Server her und loggt sich dort mit den bereitgestellten Daten ein."
  (let-multiple-getf data (:hostname hostname :jid-domain-part jid-domain-part
				     :username username :resource resource
				     :password password :mechanism mechanism)
    ;; Verbindung herstellen und bei connection speichern
    (setf (connection comm) (xmpp:connect :hostname hostname :jid-domain-part jid-domain-part :class 'xmpp-skat-connection))
    ;; Rückverweis im Verbindungsobjekt setzen (wichtig für xmpp:handle)
    (setf (comm-object (connection comm)) comm)
    ;; einloggen
    (let ((auth-result (xmpp:auth (connection comm) username password resource :mechanism mechanism)))
      (typecase auth-result
	(xmpp:xmpp-protocol-error-cancel (error 'login-unsuccessful :additional-information auth-result))))
    ;; der Vollständigkeit halber die Ressource merken, alles andere steckt in connection
    (setf (resource comm) resource
	  (address comm) (format nil "~a@~a/~a" username jid-domain-part resource))
    ;; kernel die eigene Adresse mitteilen
    (push-request comm comm 'own-address (list (address comm))))
  ;; Host instruieren, was für die Registrierung benötigt wird
  (push-request comm comm 'registration-parameters (list '((host-address string "JID des Skat-Hostes")))))

(defun make-xmpp-registration-data (host-address)
  "Erstellt eine Liste mit Registrierungsargumenten für XMPP-Comm,
zum Aufruf von REGISTRATION-DATA Handlern und comm:register."
  (list :host-address host-address))  
 
;; Das hier ist nur notwendig, damit comm seine speziellen Datensätze auspacken kann
;; (die sind ja von comm zu comm verschieden und das Gedöns soll nicht in den Kernel)
(defmethod register ((comm xmpp-comm) data)
  "Schickt dem Host die Registrierungsanfrage."
  (let-multiple-getf data (:host-address host-address)
    (send comm host-address 'registration-request)))

(defmethod send ((comm xmpp-comm) address request-name &rest request-args)
  "Sendet eine Anfrage an einen anderen Spieler. Das Format der Anfrage ist eine Liste: (cons request-name request-args)."
  (xmpp:message (connection comm) address (format nil "~s" (cons request-name request-args))))

(defmethod stop ((comm xmpp-comm))
  "Signalisiert dem XMPP-Kommunikationsmodul die Arbeit einzustellen.
Beendet die XMPP-Verbindung."
  (xmpp:disconnect (connection comm)))

(defmethod receive-requests ((comm xmpp-comm))
  "Lässt alle wartenden Anfragen abrufen"
  (when (slot-boundp comm 'connection)	; nur wenn die XMPP-Verbindung schon besteht
    (with-slots (connection) comm
      (loop while (xmpp:stanza-waiting-p connection)
	 do (xmpp:receive-stanza connection)))))

(defmethod xmpp:handle ((connection xmpp-skat-connection) (message xmpp:message))
  "Behandelt eingehende XMPP-Nachrichten-Stanzas. Wenn die Nachricht eine Liste ist, wird sie in die Queue gepackt. 
Sonst wird received-other-content signalisiert."
  (let ((read-message (handler-case (read-from-string (xmpp:body message))
			(end-of-file () 'malformed)))
	(sender (xmpp:from message)))
    (format *debug-io* "~%~a handles ~a" (address (comm-object connection)) message)
    (if (listp read-message)
	(push-request (comm-object connection) sender (car read-message) (cdr read-message))
	(signal 'received-other-content :message message))))

;; TODO: XMPP-Zeugs testen
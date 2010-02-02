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

(kern:define-login-data xmpp-login-data
    (username nil :type string) (hostname nil :type string) (domain nil :type string)
    (resource nil :type string) (password nil :type string) (mechanism nil :type (or (eql :sasl-plain) (eql :plain))))

(kern:define-registration-data xmpp-registration-data
    (host-jid nil :type string))

(defmethod host-address ((xmpp-comm xmpp-comm) (xmpp-registration-data xmpp-registration-data))
  "Extrahiert die Host-Adresse aus den XMPP-Registrierungsdaten. Gibt einfach nur die Host-JID zurück."
  (xmpp-registration-data-host-jid xmpp-registration-data))

(defmethod start ((comm xmpp-comm))
  "Initialisiert dieses XMPP-Verbindungsobjekt. Fügt nur die Login-Parameterliste für Kernel zum Abruf ein."
  (push-request comm comm 'login-struct 'xmpp-login-data)
  (values))

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
  ;; data ist vom Typ xmpp-login-data (struct) 
  (let ((hostname (xmpp-login-data-hostname data))
	(jid-domain-part (xmpp-login-data-domain data))
	(username (xmpp-login-data-username data))
	(resource (xmpp-login-data-resource data))
	(password (xmpp-login-data-password data))
	(mechanism (xmpp-login-data-mechanism data)))
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
 
;; Das hier ist nur notwendig, damit comm seine speziellen Datensätze auspacken kann
;; (die sind ja von comm zu comm verschieden und das Gedöns soll nicht in den Kernel)
(defmethod register ((comm xmpp-comm) data)
  "Schickt dem Host die Registrierungsanfrage."
  (let-multiple-getf data (:host-address host-address)
    (send comm host-address 'registration-request)))

(defmethod host-address ((comm xmpp-comm) data)
  "Gibt die Host-Adresse aus den Registrierungsdaten zurück."
  (let-multiple-getf data (:host-address host-address)
    host-address))

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
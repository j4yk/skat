(in-package skat-comm)

(defclass xmpp-skat-connection (xmpp:connection)
  ((comm-object :accessor comm-object)))

(defclass xmpp-comm (base-comm)
  ((connection :accessor connection)
   (stop-working :accessor stop-working :initform nil)
   (resource :accessor resource)
   (own-address :accessor own-address)
   (address-compare-function :initform #'equalp))
  (:documentation "Communication over an XMPP-Connection. The addresses are JID-strings."))

(defmethod start ((comm xmpp-comm))
  "Initialisiert dieses XMPP-Verbindungsobjekt. Fügt nur die Login-Parameterliste für Kernel zum Abruf ein."
  (push-request comm comm 'login-parameters 
		'((username string "XMPP Benutzername") 
		  (hostname string "Adresse des XMPP Servers") 
		  (jid-domain-part string "Domänen-Teil der JID (z. B. \"bar.org\" in \"foo@bar.org\"), falls verschieden von der Serveradresse") 
		  (resource string "XMPP-Ressource zum Einloggen")
		  (password string "Passwort")
		  (mechanism (:plain :digest-md5) "Form, in der das Passwort übermittelt wird")))
  (values))

(defmacro let-from-assoc-list ((assoc-list &rest vars) &body body)
  "Erzeugt ein let mit den Variablen vars, deren Werte aus aus assoc-list gewonnen werden.
assoc-list ist eine Liste von cons-Zellen. Die Variable-Wert-Paare werden mit assoc gefunden."
  `(let ,(loop for name in vars
	       collect `(,name (cdr (assoc ',name ,assoc-list))))
     ,@body))

(define-condition login-unsuccessful (error)
  ((additional-information :accessor additional-information :initarg :additional-information)))

(defmethod login ((comm xmpp-comm) data)
  "Stellt die XMPP-Verbindung zum Server her und loggt sich dort mit den bereitgestellten Daten ein."
  (let-from-assoc-list (data username hostname jid-domain-part resource password mechanism)
    ;; Verbindung herstellen und bei connection speichern
    (setf (connection comm) (xmpp:connect-tls :hostname hostname :jid-domain-part jid-domain-part :class 'xmpp-skat-connection))
    ;; Rückverweis im Verbindungsobjekt setzen (wichtig für xmpp:handle)
    (setf (comm-object (connection comm)) comm)
    ;; einloggen
    (let ((auth-result (xmpp:auth (connection comm) username password resource :mechanism mechanism)))
      (typecase auth-result
	(xmpp:xmpp-protocol-error-cancel (error 'login-unsuccessful :additional-information auth-result))))
    ;; der Vollständigkeit halber die Ressource merken, alles andere steckt in connection
    (setf (resource comm) resource
	  (own-address comm) (format nil "~a@~a/~a" username jid-domain-part resource))
    ;; kernel die eigene Adresse mitteilen
    (push-request comm comm 'own-address (own-address comm)))
  ;; Host instruieren, was für die Registrierung benötigt wird
  (push-request comm comm 'registration-parameters '((host-address string "JID des Skat-Hostes"))))

;; Ist das hier notwendig? Sollte eigentlich Sache des Kernels sein.
;; (defmethod register ((comm xmpp-comm) data)
;;   "Fragt bei einem Host die Registrierung an."
;;   (let-from-assoc-list (data host-address)
;;     (send comm host-address 'registration-request)))

(defmethod send ((comm xmpp-comm) address request-name &rest request-args)
  "Sendet eine Anfrage an einen anderen Spieler. Das Format der Anfrage ist eine Liste: (cons request-name request-args)."
  (xmpp:message (connection comm) address (format nil "~s" (cons request-name request-args))))

(defmethod stop ((comm xmpp-comm))
  "Signalisiert dem XMPP-Kommunikationsmodul die Arbeit einzustellen.
Beendet die XMPP-Verbindung."
  (xmpp:disconnect (connection comm)))

(defmethod receive-requests ((comm xmpp-comm))
  "Lässt alle wartenden Anfragen abrufen"
  (with-slots (connection) comm
    (loop while (xmpp:stanza-waiting-p connection)
       do (xmpp:receive-stanza connection))))

(defmethod xmpp:handle ((connection xmpp-skat-connection) (message xmpp:message))
  "Behandelt eingehende XMPP-Nachrichten-Stanzas. Wenn die Nachricht eine Liste ist, wird sie in die Queue gepackt. 
Sonst wird received-other-content signalisiert."
  (let ((read-message (handler-case (read-from-string (xmpp:body message))
			(end-of-file () 'malformed)))
	(sender (xmpp:from message)))
    (if (listp read-message)
	(push-request (comm-object connection) sender (car read-message) (cdr read-message))
	(signal 'received-other-content :message message))))

;; TODO: XMPP-Zeugs testen
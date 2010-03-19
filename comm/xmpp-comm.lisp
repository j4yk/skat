(in-package skat-comm)

(defclass xmpp-skat-connection (xmpp:connection)
  ((comm-object :accessor comm-object)))

(defclass xmpp-comm (base-comm)
  ((connection :accessor connection)
   (comm-lock :accessor lock :initform (bt:make-lock "XMPP Comm lock"))
   (receive-stanza-loop-thread :accessor receive-stanza-loop-thread)
   (stop-working :accessor stop-working :initform nil)
   (resource :accessor resource)
   (address :accessor address)
   (address-compare-function :initform #'equalp))
  (:documentation "Communication over an XMPP-Connection. The addresses are JID-strings."))

(defmacro require-lock-for-method (generic-function-name lambda-list)
  "Defines a method that aquires the lock accessed with #'lock on the first parameter
and calls the next method"
  `(defmethod ,generic-function-name ,lambda-list
     "Aquires the lock and calls the next method"
     (bt:with-lock-held ((lock ,(caar lambda-list)))
       (call-next-method))))

(require-lock-for-method push-request ((comm xmpp-comm) sender request-name request-args))
(require-lock-for-method prepend-request ((comm xmpp-comm) sender request-name request-args))
(require-lock-for-method get-request ((comm xmpp-comm)))
(require-lock-for-method has-request ((comm xmpp-comm)))

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
  (push-request comm comm 'login-struct (list 'xmpp-login-data))
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
  ((additional-information :accessor additional-information :initarg :additional-information))
  (:report (lambda (condition stream)
	     (case (additional-information condition)
	       (:timeout (format stream "Login was unsuccessful.  Reason: connection attempt timed out"))
	       (:host-not-found (format stream "Login was unsuccessful.  Reason: hostname could not be resolved"))
	       (t (let ((*print-escape* nil))
		    (format stream "Login was unsuccessful.  Reason: see additional-information ~s"
			    (additional-information condition))))))))

(defun connect (hostname &optional domain)
  (handler-case
      (with-timeout (10)
	(xmpp:connect-tls :hostname hostname :jid-domain-part domain :class 'xmpp-skat-connection))
    (timeout-error ()
      (error 'login-unsuccessful :additional-information :timeout))
    #+sbcl
    (sb-bsd-sockets:name-service-error ()
      (error 'login-unsuccessful :additional-information :host-not-found))
    (error (c) (error 'login-unsuccessful :additional-information c))))

(defmethod login ((comm xmpp-comm) data)
  "Stellt die XMPP-Verbindung zum Server her und loggt sich dort mit den bereitgestellten Daten ein."
  ;; data ist vom Typ xmpp-login-data (struct) 
  (let ((hostname (xmpp-login-data-hostname data))
	(jid-domain-part (let ((d (xmpp-login-data-domain data)))
			   (if (string= d "")
			       nil
			       d)))
	(username (xmpp-login-data-username data))
	(resource (xmpp-login-data-resource data))
	(password (xmpp-login-data-password data))
	(mechanism (xmpp-login-data-mechanism data)))
    ;; Verbindung herstellen und bei connection speichern
    (setf (connection comm) (connect hostname jid-domain-part))
    ;; Rückverweis im Verbindungsobjekt setzen (wichtig für xmpp:handle)
    (setf (comm-object (connection comm)) comm)
    ;; einloggen
    (let ((auth-result (xmpp:auth (connection comm) username password resource :mechanism mechanism)))
      (typecase auth-result
	(xmpp:xmpp-protocol-error-cancel (error 'login-unsuccessful :additional-information auth-result))))
    ;; der Vollständigkeit halber die Ressource merken, alles andere steckt in connection
    (setf (resource comm) resource
	  (address comm) (format nil "~a@~a/~a" username (or jid-domain-part hostname) resource))
    ;; kernel die eigene Adresse mitteilen
    (push-request comm comm 'own-address (list (address comm))))
  ;; Host instruieren, was für die Registrierung benötigt wird
  (push-request comm comm 'registration-struct (list 'xmpp-registration-data))
  ;; receive-stanza-loop in anderem Thread starten
  (setf (receive-stanza-loop-thread comm)
	(bt:make-thread (lambda () (xmpp:receive-stanza-loop (connection comm))) :name "XMPP Thread")))
 
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
  (xmpp:stop-stanza-loop (connection comm))
  ;; send something to let the thread return from receive-stanza
  ;; (the content of the message is equal)
  (xmpp:message (connection comm) (address comm) "Terminate")
  ;; wait for this thread to finish
  (bt:join-thread (receive-stanza-loop-thread comm))
  (xmpp:disconnect (connection comm)))

(defmethod receive-requests ((comm xmpp-comm))
  "Does nothing because receive-stanza-loop runs in another thread")

(defmethod xmpp:handle ((connection xmpp-skat-connection) (message xmpp:message))
  "Behandelt eingehende XMPP-Nachrichten-Stanzas. Wenn die Nachricht eine Liste ist, wird sie in die Queue gepackt. 
Sonst wird received-other-content signalisiert.
note: this is executed in another thread!"
  (let ((read-message (handler-case (read-from-string (xmpp:body message))
			(end-of-file () 'malformed)))
	(sender (xmpp:from message)))
    (format *debug-io* "~%~a handles message from ~a: ~a" (address (comm-object connection))
	    (xmpp:from message) (xmpp:body message))
    (if (listp read-message)
	(push-request (comm-object connection) sender (car read-message) (cdr read-message))
	(signal 'received-other-content :message message))))

;; TODO: XMPP-Zeugs testen
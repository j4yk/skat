(in-package skat-kernel)

;;; Login-Data

(defstruct login-data "Basisklasse für alle möglichen login-data structs.")

(defvar *struct-slots* nil "Assoc-Liste mit den Slot-Definitionen")

(defun slots-of-class (classname)
  "Gibt die Liste der Slot-Definitionen für diesen Struct-Typ zurück,
wenn er mit define-struct-with-slot-dict definiert wurde.
Das trifft auf mit define-login-data und define-registration-data
definierte Structs."
  (if (assoc classname *struct-slots*)
      (cdr (assoc classname *struct-slots*))))

(defstruct (example-login-data (:include login-data)) "Beispiel für eine login-data Struktur."
	   (username nil :type string) (password nil :type string))

(push '(example-login-data (username nil :type string) (password nil :type string)) *struct-slots*)

(defparameter *example-login-data-translations*
  '((:de-de (username "Benutzername" "Beschreibung des Feldes Benutzername")
     (password "Passwort" "Beschreibung des Feldes Passwort"))
    (:en-gb (username "User name" "Description of the field User Name")
     (password "Password" "Description of the field Password"))))

(defun register-slots-in-slot-dict (structure-type-name slot-descriptions)
  "Ersetzt eine vorhandene Slotliste oder fügt eine neue für diesen Typ hinzu."
  (if (member structure-type-name *struct-slots* :key #'car)
      (setf (cdr (find structure-type-name *struct-slots* :key #'car))
	    slot-descriptions)
      (push (cons structure-type-name slot-descriptions) *struct-slots*)))

(deftest "register-slots-in-slot-dict" :category "login-and-registration"
	 :test-fn (lambda ()
		    ;; erst den Testtypen entfernen
		    (loop while (member '%test-type *struct-slots* :key #'car)
			 do (setf *struct-slots* (delete '%test-type *struct-slots* :key #'car)))
		    ;; jetzt eine neue Definition
		    (register-slots-in-slot-dict '%test-type `((testslot1 nil :type t)
							       (testslot2 25 :type number)))
		    (slots-of-class '%test-type))
	 :output-form `((testslot1 nil :type t)
			(testslot2 25 :type number)))

(deftest "register-slots-in-slot-dict overwrite" :category "login-and-registration"
	 :test-fn (lambda ()
		    (register-slots-in-slot-dict '%test-type `((testslot1 nil :type t)
							      (testslot2 25 :type number)))
		    (register-slots-in-slot-dict '%test-type `((foo 'bar)))
		    (slots-of-class '%test-type))
	 :output-form `((foo 'bar)))

(defmacro define-struct-with-slot-dict (name-and-options &rest slot-descriptions)
  "Definiert einen neuen Struct und pusht die Slots in das slot-dict."
  (let ((pure-slots (labels ((maybe-remove-docstring (slots)
			       (if (stringp (car slots))
				   (maybe-remove-docstring (cdr slots))
				   slots)))
		      ;; nur die Slot-Definitionen ohne Docstring
		      (maybe-remove-docstring slot-descriptions))))
  `(progn
     (defstruct ,name-and-options ,@slot-descriptions) ; defstruct
     (register-slots-in-slot-dict
      ',(if (listp name-and-options)
	    (car name-and-options)
	    name-and-options) ',pure-slots)))) ; und Slots eintragen

(defmacro define-login-data (name &rest slot-descriptions)
  "Definiert eine neue Login-Datenstruktur"
  `(define-struct-with-slot-dict (,name (:include login-data)) ,@slot-descriptions))

;;; Registration-Data

(defstruct registration-data "Basisklasse für alle möglichen registration-data structs.")

(defmacro define-registration-data (name &rest slot-descriptions)
  "Definiert eine neue Registrierungs-Datenstruktur."
  `(define-struct-with-slot-dict (,name (:include registration-data)) ,@slot-descriptions))

;;; Tests

(deftest "define-login-data"
    :category "login-and-registration"
    :test-fn #'(lambda ()
		 (equalp 
		  (macroexpand '(define-login-data test "Teststruct" (test nil :type t)))
		  '(progn
		    (defstruct (test (:include login-data)) "Teststruct" (test nil :type t))
		    (register-slots-in-slot-dict 'test `((test nil :type t)))))))

(deftest "define-registration-data"
    :category "login-and-registration"
    :test-fn #'(lambda ()
		 (equalp 
		  (macroexpand '(define-registration-data test "Teststruct" (test nil :type t)))
		  '(progn
		    (defstruct (test (:include registration-data)) "Teststruct" (test nil :type t))
		    (register-slots-in-slot-dict 'test `((test nil :type t)))))))

(deftest "slot-dict"
    :category "login-and-registration"
    :test-fn #'(lambda ()
		 (define-struct-with-slot-dict ___test a (b 5) (c nil :type t))
		 (slots-of-class '___test))
    :output-form '(a (b 5) (c nil :type t)))

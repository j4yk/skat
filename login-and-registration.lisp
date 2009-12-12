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

(push '(example-login-data (username nil :type string) (password nil :type string)) *login-data-slots*)

(defparameter *example-login-data-translations*
  '((:de-de (username "Benutzername" "Beschreibung des Feldes Benutzername")
     (password "Passwort" "Beschreibung des Feldes Passwort"))
    (:en-gb (username "User name" "Description of the field User Name")
     (password "Password" "Description of the field Password"))))

(defmacro define-struct-with-slot-dict (name-and-options &rest slot-descriptions)
  "Definiert einen neuen Struct und pusht die Slots in das slot-dict."
  (let ((pure-slots (labels ((shrink (slots)
			       (if (stringp (car slots))
				   (shrink (cdr slots))
				   slots)))
		      ;; nur die Slot-Definitionen ohne Docstring
		      (shrink slot-descriptions))))
  `(progn
     (defstruct ,name-and-options ,@slot-descriptions) ; defstruct
     (push '(,(if (listp name-and-options)
		  (car name-and-options)
		  name-and-options) ,@pure-slots) *struct-slots*)))) ; und Slots eintragen

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
		    (push '(test (test nil :type t)) *struct-slots*)))))

(deftest "define-registration-data"
    :category "login-and-registration"
    :test-fn #'(lambda ()
		 (equalp 
		  (macroexpand '(define-registration-data test "Teststruct" (test nil :type t)))
		  '(progn
		    (defstruct (test (:include registration-data)) "Teststruct" (test nil :type t))
		    (push '(test (test nil :type t)) *struct-slots*)))))

(deftest "slot-dict"
    :category "login-and-registration"
    :test-fn #'(lambda ()
		 (define-struct-with-slot-dict ___test a (b 5) (c nil :type t))
		 (slots-of-class '___test))
    :output-form '(a (b 5) (c nil :type t)))

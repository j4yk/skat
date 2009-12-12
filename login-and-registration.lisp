(in-package skat-kernel)

;;; Login-Data

(defstruct login-data "Basisklasse für alle möglichen login-data structs.")

(defvar *login-data-slots* nil "Assoc-Liste mit den Slot-Definitionen")

(defstruct (example-login-data (:include login-data)) "Beispiel für eine login-data Struktur."
	   (username nil :type string) (password nil :type string))

(push '(example-login-data (username nil :type string) (password nil :type string)) *login-data-slots*)

(defparameter *example-login-data-translations*
  '((:de-de (username "Benutzername" "Beschreibung des Feldes Benutzername")
     (password "Passwort" "Beschreibung des Feldes Passwort"))
    (:en-gb (username "User name" "Description of the field User Name")
     (password "Password" "Description of the field Password"))))

(defmacro define-struct-with-slot-dict (name-and-options slot-dict &rest slot-descriptions)
  "Definiert einen neuen Struct und pusht die Slots in das slot-dict."
  (let ((pure-slots (labels ((shrink (slots)
			       (if (listp (car slots))
				   slots
				   (shrink (cdr slots)))))
		      ;; nur die Slot-Definitionen ohne Docstring
		      (shrink slot-descriptions))))
  `(progn
     (defstruct ,name-and-options ,@slot-descriptions) ; defstruct
     (push '(,(if (listp name-and-options)
		  (car name-and-options)
		  name-and-options) ,@pure-slots) ,slot-dict)))) ; und Slots eintragen

(defmacro define-login-data (name &rest slot-descriptions)
  "Definiert eine neue Login-Datenstruktur"
  `(define-struct-with-slot-dict (,name (:include login-data))
       *login-data-slots* ,@slot-descriptions))

;;; Registration-Data

(defstruct registration-data "Basisklasse für alle möglichen registration-data structs.")

(defvar *registration-data-slots* nil "Assoc-Liste mit den Slot-Definitionen")

(defmacro define-registration-data (name &rest slot-descriptions)
  "Definiert eine neue Registrierungs-Datenstruktur."
  `(define-struct-with-slot-dict (,name (:include registration-data))
       *registration-data-slots* ,@slot-descriptions))

(deftest "define-login-data"
    :category "login-and-registration"
    :test-fn #'(lambda ()
		 (equalp 
		  (macroexpand '(define-login-data test "Teststruct" (test nil :type t)))
		  '(progn
		    (defstruct (test (:include login-data)) "Teststruct" (test nil :type t))
		    (push '(test (test nil :type t)) *login-data-slots*)))))

(deftest "define-registration-data"
    :category "login-and-registration"
    :test-fn #'(lambda ()
		 (equalp 
		  (macroexpand '(define-registration-data test "Teststruct" (test nil :type t)))
		  '(progn
		    (defstruct (test (:include registration-data)) "Teststruct" (test nil :type t))
		    (push '(test (test nil :type t)) *registration-data-slots*)))))
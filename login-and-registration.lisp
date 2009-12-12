(in-package skat-kernel)

(defstruct login-data "Basisklasse für alle möglichen login-data structs.")

(defstruct (example-login-data (:include login-data)) "Beispiel für eine login-data Struktur." username password)

(defparameter *example-login-data-translations*
  '((:de-de (username "Benutzername" "Beschreibung des Feldes Benutzername")
     (password "Passwort" "Beschreibung des Feldes Passwort"))
    (:en-gb (username "User name" "Description of the field User Name")
     (password "Password" "Description of the field Password"))))

(defmacro define-login-data (name &rest slot-descriptions)
  "Definiert eine neue Login-Datenstruktur"
  `(defstruct (,name (:include login-data)) ,@slot-descriptions))

(defstruct registration-data "Basisklasse für alle möglichen registration-data structs.")

(defmacro define-registration-data (name &rest slot-descriptions)
  "Definiert eine neue Registrierungs-Datenstruktur."
  `(defstruct (,name (:include registration-data)) ,@slot-descriptions))

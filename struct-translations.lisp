(in-package skat-kernel)

(defun translations-variable-name (type)
  (intern (format nil "*~a-TRANSLATIONS*" (class-name (find-class type)))))

(defun translations (type)
  (eval (translations-variable-name type)))

(defun slot-translation (class-name slot-name locale-keyword)
  "Ruft die Übersetzungen für diesen Slot für die Benutzerschnittstelle ab."
  (assoc slot-name (cdr (assoc locale-keyword (translations class-name)))))

(defun add-translation (class-name slot-name locale-keyword slot-label slot-description)
  (if (boundp (translations-variable-name class-name))
      ;; Übersetzungstabelle schön eröffnet
      (if (member locale-keyword (mapcar #'car (translations class-name)))
	  ;; locale schon definiert
	  (let ((locale-list (cdr (assoc locale-keyword (translations class-name)))))
	    (if (member slot-name (mapcar #'car locale-list))
		;; Slot schon dokumentiert, ersetzen
		(rplacd (slot-translation class-name slot-name locale-keyword) (list slot-label slot-description))
		;; Slot noch nicht dokumentiert
		(push (list slot-name slot-label slot-description) (cdr (assoc locale-keyword (translations class-name))))))
	  ;; locale noch nicht definiert
	  (push (list locale-keyword (list slot-name slot-label slot-description)) (symbol-value (translations-variable-name class-name))))
      ;; Übersetzungstabelle noch nicht eröffnet
      (setf (symbol-value (translations-variable-name class-name))
	    (list (list locale-keyword (list slot-name slot-label slot-description))))))

(defun slot-label (class-name slot-name locale-keyword)
  "Ruft die Beschriftung des Slots für die Benutzerschnittstelle ab."
  (second (slot-translation class-name slot-name locale-keyword)))

(defun slot-description (class-name slot-name locale-keyword)
  "Ruft die Beschreibung des Slots für die Benutzerschnittstelle ab."
  (third (slot-translation class-name slot-name locale-keyword)))

(deftests "Übersetzungen"
  ("Label abrufen" (slot-label 'example-login-data 'password :de-de) "Passwort")
  ("Beschreibung abrufen" (slot-description 'example-login-data 'password :de-de) "Beschreibung des Feldes Passwort"))

(deftest "add-translation neue locale" :category "Übersetzungen"
	 :test-fn #'(lambda ()
		      (let ((stub-locale (gensym)))
			(add-translation 'example-login-data 'username stub-locale "Test" "Testbeschreibung")
			(slot-translation 'example-login-data 'username stub-locale)))
	 :output-form '(username "Test" "Testbeschreibung") :compare-fn #'equalp)

(defvar *__add-translation-test-stub-slot* (gensym)
  "Dummy-Variable für den Testfall add-translation neuer slot")
(deftest "add-translation neuer slot" :category "Übersetzungen"
	 :test-fn #'(lambda ()
		      (defparameter *__add-translation-test-stub-slot* (gensym))
		      (add-translation 'example-login-data *__add-translation-test-stub-slot* :de-de "Test" "Testbeschreibung")
		      (slot-translation 'example-login-data *__add-translation-test-stub-slot* :de-de))
	 :output-form (list *__add-translation-test-stub-slot* "Test" "Testbeschreibung")
	 :compare-fn #'equalp)

(deftest "add-translation existierender slot" :category "Übersetzungen"
	 :test-fn #'(lambda ()
		      (add-translation 'example-login-data 'username :de-de "Test" "Testbeschreibung")
		      (slot-translation 'example-login-data 'username :de-de))
	 :output-form '(username "Test" "Testbeschreibung"))

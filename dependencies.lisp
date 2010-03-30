(defpackage skat-installer
  (:nicknames :installer)
  (:use :cl)
  (:export #:ensure-dependencies
	   #:ensure-skat-dependencies))

(in-package :skat-installer)

(require 'asdf-install)

(defun install-dependencies (packages)
  ;; install all packages locally, i. e. enter "2" when asdf-install asks where to install
  (let ((personal-installation-code (write-to-string (1+ (position "Personal installation" asdf-install::*locations*
								   :key (lambda (l) (nth 2 l)) :test #'string=)))))
    (with-input-from-string (input (format nil "~{~a~%~}" 
					   (loop for i from 1 to (length packages)
					      collect personal-installation-code)))
      (let ((*standard-input* input))
      ;; skip GPG checks, don't interrupt
	(flet ((skip-gpg-check (&optional error)
		 (declare (ignore error))
		 (when (find-restart 'asdf-install::skip-gpg-check)
		   (invoke-restart 'asdf-install::skip-gpg-check))))
	  (handler-bind ((asdf-install::download-error #'skip-gpg-check)
			 (asdf-install::key-not-trusted #'skip-gpg-check)
			 (asdf-install::key-not-found #'skip-gpg-check))
	    (apply #'asdf-install:install packages)))))))

(defun ensure-dependencies (packages)
  (let ((missing (loop for p in packages
		    unless (asdf:find-system p nil)
		    collect p)))
    (when missing
      (install-dependencies missing))))

(defun ensure-skat-dependencies ()
  (ensure-dependencies '(clunit fiveam
			 cffi trivial-garbage trivial-timeout bordeaux-threads
			 cl-xmpp lispbuilder-sdl lispbuilder-sdl-image cl-opengl)))

(eval-when (:execute)
  (ensure-skat-dependencies)

  (mapcar #'require '(clunit fiveam cffi trivial-garbage
		      trivial-timeout bordeaux-threads cl-xmpp
		      lispbuilder-sdl lispbuilder-sdl-image
		      cl-opengl))

  (save-lisp-and-die "dependencies.core"))

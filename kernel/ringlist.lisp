(in-package :skat-kernel)

(defun make-ring (proper-list)
  "Wandelt eine Kopie der normalen Liste in eine geschlossene Ringliste um.
Nebeneffekte: setzt *print-circle* auf t."
  (setq *print-circle* t)
  (let* ((list (copy-list proper-list)))
    (setf (cdr (last list)) list) ; Ringschluss
    list))

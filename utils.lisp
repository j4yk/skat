(in-package :skat-utils)

(defun to-keyword (symbol)
  "Gibt ein Symbol gleichen Namens aus dem Package keyword zurück"
  (intern (symbol-name symbol) :keyword))

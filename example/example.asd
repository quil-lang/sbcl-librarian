;;;; An example system.

(asdf:defsystem #:libcalc
  :description "Libcalc tool."
  :author "Charles Zhang <czhang@hrl.com>"
;  :depends-on (#:sbcl-librarian)
  :serial t
  :components ((:file "package")
               (:file "libcalc")
               (:file "bindings")))

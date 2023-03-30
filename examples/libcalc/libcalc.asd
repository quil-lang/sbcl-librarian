(asdf:defsystem #:libcalc
  :description "Libcalc tool."
  :author "Charles Zhang <czhang@hrl.com>"
  :defsystem-depends-on (#:sbcl-librarian)
  :depends-on (#:sbcl-librarian)
  :serial t
  :components ((:file "package")
               (:file "libcalc")
               (:file "bindings")))

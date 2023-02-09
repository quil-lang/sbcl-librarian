(asdf:defsystem #:consplosion
  :description "Trying to Force a Garbage Strike"
  :author "Colin O'Keefe <cbokeefe@hrl.com>"
  :defsystem-depends-on (#:sbcl-librarian)
  :depends-on (#:sbcl-librarian)
  :serial t
  :components ((:file "package")
               (:file "consplosion")
               (:file "bindings")))

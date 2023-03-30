(asdf:defsystem #:libcallback
  :description "callback example."
  :author "Colin OKeefe <cbokeefe@hrl.com>"
  :defsystem-depends-on (#:sbcl-librarian)
  :depends-on (#:sbcl-librarian)
  :serial t
  :components ((:file "package")
               (:file "bindings")))

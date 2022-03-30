(asdf:defsystem #:libsquare
  :description "Dummy library."
  :author "Kartik Singh <kssingh@hrl.com>"
  :defsystem-depends-on (#:sbcl-librarian)
  :depends-on (#:sbcl-librarian)
  :serial t
  :components ((:file "package")
               (:file "libsquare")
               (:file "bindings")))

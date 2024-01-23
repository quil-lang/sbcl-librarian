;;;; libbase.asd

(asdf:defsystem #:libbase
  :description "A shared library interface for the built-in SBCL-LIBRARIAN APIs."
  :author "Kartik Singh <kssingh@hrl.com>"
  :depends-on (#:sbcl-librarian
               )
  :serial t
  :components ((:file "package")
               (:file "libbase")))

;;;; libminiqvm.asd

(asdf:defsystem #:libminiqvm
  :description "A simplified shared library interface for QVM."
  :author "Kartik Singh <kssingh@hrl.com"
  :depends-on (#:qvm
               #:sbcl-librarian)
  :serial t
  :components ((:file "package")
               (:file "libminiqvm")))

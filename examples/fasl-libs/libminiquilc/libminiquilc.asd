;;;; libminiquilc.asd

(asdf:defsystem #:libminiquilc
  :description "A simplified shared library interface for QUILC."
  :author "Kartik Singh <kssingh@hrl.com>"
  :depends-on (#:cl-quil
               #:sbcl-librarian
               )
  :serial t
  :components ((:file "package")
               (:file "libminiquilc")))

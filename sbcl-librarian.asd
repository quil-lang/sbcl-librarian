;;;; sbcl-librarian.asd

(asdf:defsystem #:sbcl-librarian
  :description "Tool for generating Lisp bindings."
  :author "Charles Zhang <czhang@hrl.com>"
  ; :license "TODO"
  ; :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "handles")
               (:file "sbcl-librarian")))

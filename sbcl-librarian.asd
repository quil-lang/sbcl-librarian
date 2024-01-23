;;;; sbcl-librarian.asd

(asdf:defsystem #:sbcl-librarian
  :description "Tool for generating Lisp bindings."
  :author "Charles Zhang <czhang@hrl.com>"
  ; :license "TODO"
  ; :version "0.0.1"
  :in-order-to ((test-op (test-op "sbcl-librarian/tests")))
  :depends-on (#:sb-sprof
               #:swank
               )
  :serial t
  :pathname "src/"
  :components ((:file "package")
               (:file "asdf-utils")
               (:file "util")
               (:file "types")
               (:file "function")
               (:file "api")
               (:file "conditions")
               (:file "errors")
               (:file "library")
               (:file "bindings")
               (:file "python-bindings")
               (:file "handles")
               (:file "loader")
               (:file "environment")
               (:file "fasl-lib")
               (:file "diagnostics")
               ))

(asdf:defsystem #:sbcl-librarian/project
  :description "Project skeleton builder for SBCL-LIBRARIAN"
  :author "Colin O'Keefe <cbokeefe@hrl.com>"
  :depends-on (#:alexandria)
  :serial t
  :pathname "src/project/"
  :components ((:file "package")
               (:file "project")))

(asdf:defsystem #:sbcl-librarian/tests
  :description "Tests for sbcl-librarian."
  :author "Kartik Singh <kssingh@hrl.com>"
  :depends-on (#:sbcl-librarian
               #:fiasco)
  :perform (test-op (o s)
                    (uiop:symbol-call ':sbcl-librarian.tests
                                      '#:run-test-suite))
  :serial t
  :pathname "tests/"
  :components ((:file "package")
               (:file "suite")
               (:file "build-tests")
               (:file "python-tests")))

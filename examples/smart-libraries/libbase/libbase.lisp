;;;; libbase.lisp

(in-package #:libbase)

(sbcl-librarian:define-aggregate-library libbase (:function-linkage "BASE_")
  sbcl-librarian:environment
  sbcl-librarian:handles
  sbcl-librarian:loader)

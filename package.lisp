;;;; package.lisp

(defpackage #:sbcl-librarian
  (:use #:cl)
  (:export #:define-handle-type
           #:define-enum-type
           #:define-error-map
           #:define-library))

(defpackage #:sbcl-librarian-example
  (:use #:cl #:sbcl-librarian))

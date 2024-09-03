;;;; package.lisp
(require "sb-sprof")

(defpackage #:sbcl-librarian
  (:shadow
   #:error
   #:warning
   #:assert)
  (:use #:cl)
  (:export #:define-handle-type
           #:define-enum-type
           #:define-error-map
           #:define-api
           #:define-library
           #:define-aggregate-library
           #:build-bindings
           #:build-python-bindings
           #:build-core-and-die

           #:library-c-name
           #:callable-exports

           #:loader
           #:handles
           #:environment

	   #:error
	   #:warning
	   #:bug
	   #:unreachable))


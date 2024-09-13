;;;; package.lisp
(require "sb-sprof")

(require :sb-sprof)

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
           #:create-fasl-library-cmake-project

           #:library-c-name
           #:callable-exports

           #:diagnostics
           #:environment
           #:errors
           #:handles
           #:loader

	   #:error
	   #:warning
	   #:bug
	   #:unreachable))


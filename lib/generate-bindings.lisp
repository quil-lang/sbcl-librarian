(require "asdf")

(asdf:load-system :sbcl-librarian)
(handler-bind ((deprecation-condition #'continue))
  (asdf:load-system :swank))
(in-package #:sbcl-librarian)

(define-aggregate-library libsbcl-librarian (:function-linkage "LIBSBCL_LIBRARIAN_API")
  diagnostics
  environment
  errors
  handles
  loader)

(build-bindings libsbcl-librarian "." :omit-init-function t)
(build-python-bindings libsbcl-librarian "." :omit-init-call t)
(build-core-and-die libsbcl-librarian ".")

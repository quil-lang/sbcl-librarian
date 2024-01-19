(require "asdf")

(asdf:load-system :sbcl-librarian)

(in-package #:sbcl-librarian)

(build-bindings libsbcl "." :omit-init-function t)

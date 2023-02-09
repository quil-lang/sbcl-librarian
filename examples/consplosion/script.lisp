(require '#:asdf)

(asdf:load-system '#:consplosion)

(in-package #:sbcl-librarian.tests.consplosion)

(build-bindings consplosion ".")
(build-python-bindings consplosion ".")
(build-core-and-die consplosion ".")

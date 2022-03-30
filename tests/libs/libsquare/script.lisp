(require '#:asdf)

(asdf:load-system '#:libsquare)

(in-package #:sbcl-librarian/tests/libs/libsquare)

(build-bindings libsquare ".")
(build-python-bindings libsquare ".")
(build-core-and-die libsquare ".")

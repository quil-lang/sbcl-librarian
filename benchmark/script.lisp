(require '#:asdf)

(asdf:load-system '#:benchmark)

(in-package #:sbcl-librarian/benchmark)

(build-bindings benchmark ".")
(build-python-bindings benchmark ".")
(build-core-and-die benchmark ".")

(require '#:asdf)

(asdf:load-system '#:libcalc)


(in-package #:sbcl-librarian/example/libcalc)

(build-bindings libcalc ".")
(build-python-bindings libcalc ".")
(build-core-and-die libcalc "." :compression nil)

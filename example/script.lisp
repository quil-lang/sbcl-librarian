(require '#:asdf)

(asdf:load-system '#:sbcl-librarian/example/libcalc)

(build-bindings libcalc ".")
(build-python-bindings libcalc ".")
(build-core-and-die libcalc ".")

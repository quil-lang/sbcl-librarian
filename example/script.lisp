(require '#:asdf)

(asdf:load-system '#:libcalc)

(build-bindings libcalc ".")
(build-python-bindings libcalc ".")
(build-core-and-die libcalc ".")

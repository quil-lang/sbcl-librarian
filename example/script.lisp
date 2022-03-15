(require '#:asdf)

;; Requirement for loading the .asd directly can be avoided by adding
;;
;; sbcl-librarian/example/libcalc.asd
;;
;; to the system-index.txt file.
(asdf:load-asd (truename "./libcalc.asd"))
(asdf:load-system '#:libcalc)

(in-package #:sbcl-librarian/example/libcalc)

(build-bindings libcalc ".")
(build-python-bindings libcalc ".")
(build-core-and-die libcalc ".")

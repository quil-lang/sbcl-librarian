;;;; libminiquilc.lisp

(in-package #:libminiquilc)

(sbcl-librarian:define-handle-type quil-program-type "quil_program")

(sbcl-librarian:define-library libminiquilc (:function-linkage "MINIQUILC_API"
                                             :function-prefix "miniquilc_")
  (:type quil-program-type)
  (:function
   (("parse_quil" cl-quil:safely-parse-quil) quil-program-type ((source :string)))))

;;;; libminiquilc.lisp

(in-package #:libminiquilc)

(sbcl-librarian:define-handle-type quil-program-type "quil_program")

(sbcl-librarian:define-enum-type error-type "miniquilc_err_t"
  ("MINIQUILC_ERR_SUCCESS" 0)
  ("MINIQUILC_ERR_FAIL" 1))
(sbcl-librarian:define-error-map error-map error-type 0
  ((t (lambda (condition)
        (declare (ignore condition))
        (return-from error-map 1)))))

(sbcl-librarian:define-library libminiquilc (:error-map error-map
                                             :function-linkage "MINIQUILC_API"
                                             :function-prefix "miniquilc_")
  (:type quil-program-type error-type)
  (:function
   (("parse_quil" cl-quil:safely-parse-quil) quil-program-type ((source :string)))))

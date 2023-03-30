;;; Library definition.

(in-package #:sbcl-librarian/example/libcalc)

(define-handle-type expr-type "expr_type")
(define-enum-type error-type "err_t"
  ("ERR_SUCCESS" 0)
  ("ERR_FAIL" 1))
(define-error-map error-map error-type 0
  ((t (lambda (condition)
        (declare (ignore condition))
        (return-from error-map 1)))))

(define-api libcalc-api (:error-map error-map
                         :function-prefix "calc_")
  (:literal "/* types */")
  (:type expr-type error-type)
  (:literal "/* functions */")
  (:function
   (int-literal expr-type ((value :int)))
   (int-literal-value :int ((expr expr-type)))
   (int-literal-p :bool ((obj expr-type)))
   (sum-expression expr-type ((left expr-type) (right expr-type)))
   (sum-expression-left-arg expr-type ((expr expr-type)))
   (sum-expression-right-arg expr-type ((expr expr-type)))
   (sum-expression-p :bool ((expr expr-type)))
   (simplify expr-type ((expr expr-type)))
   (parse expr-type ((source :string)))
   (expression-to-string :string ((expr expr-type)))
   (remove-zeros expr-type ((expr expr-type)))))

(define-aggregate-library libcalc (:function-linkage "CALC_API")
  sbcl-librarian:handles sbcl-librarian:environment libcalc-api)


;;; Library definition.

(in-package #:sbcl-librarian/example/libcalc)

(define-handle-type expr-type "expr_type")
(define-enum-type error-type "err_t"
  ("ERR_SUCCESS" 0)
  ("ERR_FAIL" 1)
  ("ERR_FATAL" 2))
(define-error-map error-map error-type (:no-error 0 :fatal-error 2)
  ((t (lambda (condition)
        (declare (ignore condition))
        (return-from error-map 1)))))

(defun exhaust-heap ()
  (sb-sys:without-gcing
    (let ((test '()))
      (loop (push 1 test)))))

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
   (remove-zeros expr-type ((expr expr-type)))
   (exhaust-heap :void ())))

(define-aggregate-library libcalc (:function-linkage "CALC_API")
  sbcl-librarian:handles sbcl-librarian:environment libcalc-api)


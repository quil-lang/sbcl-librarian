;;; Library definition.

(in-package #:sbcl-librarian/example/libcalc)

(sbcl-librarian:define-handle-type expr-type "expr_type")

(defun exhaust-heap ()
  (sb-sys:without-gcing
    (let ((test '()))
      (loop (push 1 test)))))

(sbcl-librarian:define-api libcalc-api (:function-prefix "calc_")
  (:literal "/* types */")
  (:type expr-type)
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

(sbcl-librarian:define-aggregate-library calc (:function-linkage "CALC_API")
  libcalc-api)


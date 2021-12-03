(require '#:asdf)

(asdf:load-system '#:sbcl-librarian)

(defpackage #:sbcl-librarian-example
  (:use #:cl #:sbcl-librarian))

(in-package #:sbcl-librarian-example)

(defclass expression ()
  ()
  (:documentation "An abstract expression."))

(defclass int-literal (expression)
  ((value :initarg :value
          :reader int-literal-value))
  (:documentation "An integer literal."))

(defun int-literal (value)
  (make-instance 'int-literal :value value))

(defun int-literal-p (obj)
  (typep obj 'int-literal))

(defclass sum-expression (expression)
  ((left-arg :initarg :left
             :reader sum-expression-left-arg)
   (right-arg :initarg :right
              :reader sum-expression-right-arg)))

(defun sum-expression (left right)
  (make-instance 'sum-expression :left left :right right))

(defun sum-expression-p (obj)
  (typep obj 'sum-expression))

(defun parse (source)
  (labels ((parse-expr (lisp-expr)
             (cond ((integerp lisp-expr)
                    (make-instance 'int-literal :value lisp-expr))
                   ((and (listp lisp-expr) (eq '+ (first lisp-expr)))
                    (let* ((left (parse-expr (second lisp-expr)))
                           (right (parse-expr (third lisp-expr))))
                      (make-instance 'sum-expression
                                     :left left
                                     :right right)))
                   (t
                    (error "Unable to parse expression: ~A" lisp-expr)))))
    (let ((*package* (find-package "SBCL-LIBRARIAN-EXAMPLE")))
      (with-input-from-string (stream source)
        (parse-expr (read stream))))))

(defparameter *print-expression-indent* 0)

(defun print-indentation (stream)
  (when (plusp *print-expression-indent*)
    (format stream "~va" *print-expression-indent* " ")))

(defgeneric print-expression (obj stream)
  (:method ((obj int-literal) stream)
    (format stream "~D" (int-literal-value obj)))
  (:method ((obj sum-expression) stream)
    (format stream "(+")
    (let ((*print-expression-indent* (+ *print-expression-indent* 2)))
      (with-slots (left-arg right-arg) obj
        (cond ((typep left-arg 'int-literal)
               (format stream " ")
               (print-expression left-arg stream)
               (format stream " "))
              (t
               (terpri stream)
               (print-indentation stream)
               (print-expression left-arg stream)
               (terpri stream)
               (print-indentation stream)))
        (print-expression right-arg stream)))
    (format stream ")")))

(defun expression-to-string (expr)
  (with-output-to-string (s)
    (print-expression expr s)))

(defun simplify (expr)
  (labels ((%eval (expr)
             (etypecase expr
               (int-literal (int-literal-value expr))
               (sum-expression
                (+ (%eval (sum-expression-left-arg expr))
                   (%eval (sum-expression-right-arg expr)))))))
    (make-instance 'int-literal
                   :value (%eval expr))))

;;; Library definition.

(define-handle-type expr-type "expr_type")
(define-enum-type error-type "err_t"
  ("ERR_SUCCESS" 0)
  ("ERR_FAIL" 1))
(define-error-map error-map error-type 0
  (t (condition) (declare (ignore condition)) 1))

(define-library libcalc (:error-map error-map
                         :function-linkage "CALC_API"
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
   (expression-to-string :string ((expr expr-type)))))


(build-bindings libcalc ".")
(build-python-bindings libcalc ".")
(build-core-and-die libcalc ".")

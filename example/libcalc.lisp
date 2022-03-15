(in-package #:sbcl-librarian/example/libcalc)

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

(defun parse-expr (lisp-expr)
  (cond ((integerp lisp-expr)
         (make-instance 'int-literal :value lisp-expr))
        ((and (listp lisp-expr) (eq '+ (first lisp-expr)))
         (let* ((left (parse-expr (second lisp-expr)))
                (right (parse-expr (third lisp-expr))))
           (make-instance 'sum-expression
                          :left left
                          :right right)))
        (t
         (error "Unable to parse expression: ~A" lisp-expr))))

(defun parse (source)
  (with-input-from-string (stream source)
    (parse-expr (read stream))))

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

(defun remove-zeros (expr)
  (labels ((%zerop (expr)
             (and (int-literal-p expr)
                  (zerop (int-literal-value expr)))))
    (etypecase expr
      (int-literal expr)
      (sum-expression
       (with-slots (left-arg right-arg) expr
         (let ((simpl-left (remove-zeros left-arg))
               (simpl-right (remove-zeros right-arg)))
           (cond ((%zerop simpl-left) simpl-right)
                 ((%zerop simpl-right) simpl-left)
                 (t (sum-expression simpl-left simpl-right)))))))))

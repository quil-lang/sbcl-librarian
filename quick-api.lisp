(in-package #:sbcl-librarian)

(defun function-spec-from-symbol (sym)
  (let ((func (fboundp sym)))
    (if (null func)
        (values)
        (let ((signature (sb-introspect:function-type func))
              (lambda-list (sb-introspect:function-lambda-list func)))
          `(,sym ,(return-type signature) ,(typed-args lambda-list signature))))))

(defun return-type (signature)
  (ffi-type (third signature)))

(defun atomic-type (type-spec)
  (etypecase type-spec
    (list
     (ecase (first type-spec)
       (values (atomic-type (second type-spec)))
       (mod 'integer)
       (not t)
       (or t)))
    (symbol type-spec)))

(defun ffi-type (type-spec)
  (case (atomic-type type-spec)
    (boolean :bool)
    (integer :int)
    (simple-string :string)
    (double-float :double)
    (otherwise :handle)))

(defun typed-args (lambda-list signature)
  (loop :for type-spec :in (second signature)
        :for name :in lambda-list
        :collect `(,name ,(ffi-type type-spec))))

(defun package-specs (package)
  (let ((specs '()))
    (do-external-symbols (sym package)
      (unless (fboundp sym)
        (format t "~a~%" sym))
      (push (function-spec-from-symbol sym) specs))
    specs))

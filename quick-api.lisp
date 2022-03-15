(in-package #:sbcl-librarian)

(defun package-specs (package handle-type)
  (labels ((function-spec-from-symbol (sym)
             (let ((func (fboundp sym)))
               (if (null func)
                   (values)
                   (let ((signature (sb-introspect:function-type func))
                         (lambda-list (sb-introspect:function-lambda-list func)))
                     `(,sym ,(return-type signature) ,(typed-args lambda-list signature))))))

           (return-type (signature)
             (ffi-type (third signature)))

           (atomic-type (type-spec)
             (etypecase type-spec
               (list
                (ecase (first type-spec)
                  (values (atomic-type (second type-spec)))
                  (mod 'integer)
                  (not t)
                  (or t)))
               (symbol type-spec)))

           (ffi-type (type-spec)
             (case (atomic-type type-spec)
               (boolean :bool)
               (integer :int)
               (simple-string :string)
               (double-float :double)
               (otherwise handle-type)))

           (typed-args (lambda-list signature)
             (loop :for type-spec :in (second signature)
                   :for name :in lambda-list
                   :collect `(,name ,(ffi-type type-spec)))))
    (let ((specs '()))
      (do-external-symbols (sym package)
        (unless (fboundp sym)
          (format t "~a~%" sym))
        (push (function-spec-from-symbol sym) specs))
      specs)))

(defmacro define-quick-api (name (&key package (function-prefix "") error-map handle-type error-type))
  (let ((specs `((:type ,error-type ,handle-type)
                 (:function ,@(package-specs package handle-type)))))
    `(progn
       ,@(callable-definitions-from-spec function-prefix error-map specs)
       (defvar ,name
         (make-instance 'api
                        :name ',name
                        :error-map ',error-map
                        :function-prefix ,function-prefix
                        :specs ',specs)))))

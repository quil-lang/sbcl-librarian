(in-package #:sbcl-librarian)

(defvar *initialize-callables-p* nil
  "Bound to T when we are loading Lisp and want to reinitialize
  callables on load.")

(defun canonical-signature (name result-type typed-lambda-list &key
                                                                 (function-prefix "")
                                                                 error-map)
  (let* ((callable-name (prefix-name function-prefix name))
         (return-type
           (if error-map
               (error-map-type error-map)
               result-type))
         (use-result-arg
           (and error-map
                (not (eq result-type ':void)))))
    (values callable-name
            return-type
            typed-lambda-list
            (and use-result-arg result-type))))

(defun c-function-declaration (name result-type typed-lambda-list
                               &key (datap 't) (externp nil)
                                    (linkage nil)
                                    (function-prefix "")
                                    error-map)
  (multiple-value-bind (callable-name return-type typed-lambda-list result-type)
      (canonical-signature name result-type typed-lambda-list
                           :function-prefix function-prefix
                           :error-map error-map)
    (format nil "~:[~;extern ~]~@[~a ~]~a ~:[~a~;(*~a)~](~{~a~^, ~})"
            externp
            linkage
            (c-type return-type)
            datap
            (coerce-to-c-name callable-name)
            (append
             (mapcar (lambda (item)
                       (destructuring-bind (name type)
                           item
                         (format nil "~a ~a" (c-type type) (lisp-to-c-name name))))
                     typed-lambda-list)
             (and result-type
                  (list (format nil "~a *result" (c-type result-type))))))))

(defun c-function-definition (name result-type typed-lambda-list
                              &key (function-prefix "")
                                error-map)
  (multiple-value-bind (callable-name return-type typed-lambda-list result-type)
      (canonical-signature name result-type typed-lambda-list
                           :function-prefix function-prefix
                           :error-map error-map)
    (format nil "~a ~a(~{~a~^, ~}) {~%~a~%}~%"
            (c-type return-type)
            (coerce-to-c-name callable-name)
            (append
             (mapcar (lambda (item)
                       (destructuring-bind (name type)
                           item
                         (format nil "~a ~a" (c-type type) (lisp-to-c-name name))))
                     typed-lambda-list)
             (and result-type
                  (list (format nil "~a *result" (c-type result-type)))))
            (format nil "    ~a(~{~a~^, ~});"
                    (concatenate 'string "_" (coerce-to-c-name callable-name))
                    (append
                     (mapcar (lambda (item)
                               (destructuring-bind (name type)
                                   item
                                 (lisp-to-c-name name)))
                             typed-lambda-list)
                     (and result-type
                          (list "result")))))))

(defun callable-definition (name result-type typed-lambda-list &key
                                                                 (function-prefix "")
                                                                 error-map)
  (let ((bindings
          (mapcar (lambda (item)
                    (destructuring-bind (arg type)
                        item
                      (list (gensym)
                            (funcall (alien-to-lisp type) arg))))
                  typed-lambda-list)))
    (multiple-value-bind (callable-name return-type typed-lambda-list result-type)
        (canonical-signature name
                             result-type
                             typed-lambda-list
                             :function-prefix function-prefix
                             :error-map error-map)
      `(progn
         (sb-alien:define-alien-callable
             ,callable-name
             ,(sb-alien-type return-type)
             (,@(loop :for (arg type) :in typed-lambda-list
                      :collect (list arg (sb-alien-type type)))
              ,@(when result-type
                  `((result (* ,(sb-alien-type result-type))))))
           (let ,bindings
             ,(let* ((wrapped
                       (funcall (lisp-to-alien (or result-type return-type))
                                `(,(if (listp name) (second name) name) ,@(mapcar #'first bindings))))
                     (result
                       (if result-type
                           `(setf (sb-alien:deref result) ,wrapped)
                           wrapped)))
                (if error-map
                    (wrap-error-handling result error-map)
                    result))))
         (when *initialize-callables-p*
           (sb-alien::initialize-alien-callable-symbol ',callable-name))))))

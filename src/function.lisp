(in-package #:sbcl-librarian)

(defparameter *longjmp-operator*
  #-win32 "longjmp" #+win32 "__builtin_longjmp"
  "The name of the function/macro/builtin that implements longjmp. Note
that we use __builtin_longjmp on Windows because, unlike the longjmp
provided by the UCRT, it does not perform stack unwinding, which does
not work with Lisp stack frames.")

(defparameter *setjmp-operator*
  #-win32 "setjmp" #+win32 "__builtin_setjmp"
  "The name of the function/macro/builtin that implements longjmp. See
the documentation for *longjmp-operator* for rationale.")

(defvar *initialize-callables-p* nil
  "Bound to T when we are loading Lisp and want to reinitialize
  callables on load.")

(defun canonical-signature (name result-type typed-lambda-list &key
                                                                 (function-prefix "")
                                                                 (c-prefix "")
                                                                 error-map)
  (let* ((callable-name (callable-name-with-c-prefix (prefix-name function-prefix name)
                                                     c-prefix))
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
                                    (c-prefix "")
                                 error-map)
  (multiple-value-bind (callable-name return-type typed-lambda-list result-type)
      (canonical-signature name result-type typed-lambda-list
                           :function-prefix function-prefix
                           :c-prefix c-prefix
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
  "Returns a string constituting a C definition for a function called
NAME that implements the function declaration produced by calling
C-FUNCTION-DECLARATION on the provided arguments. The function body
forwards the function arguments to a call to a function pointer with
the same name as the function, except with a leading underscore.

The call to the function pointer is wrapped as follows:

if (!setjmp(fatal_lisp_error_handler)) {
    // function pointer call
} else {
    return FATAL_ERROR_CODE;  // or ldb_monitor();, which
                              // drops into LDB, if ERROR-MAP does not
                              // have a FATAL-ERROR
}"
  (let ((header (c-function-declaration name result-type typed-lambda-list
                                        :datap nil :externp nil :linkage nil
                                        :function-prefix function-prefix :error-map error-map)))
    (multiple-value-bind (callable-name return-type typed-lambda-list result-type)
        (canonical-signature name result-type typed-lambda-list
                             :function-prefix function-prefix
                             :error-map error-map)
      (declare (ignore return-type))
      (let ((call-statement (format nil "return ~a(~{~a~^, ~});"
                                    (concatenate 'string "_" (coerce-to-c-name callable-name))
                                    (append
                                     (mapcar (lambda (item)
                                               (lisp-to-c-name (first item)))
                                             typed-lambda-list)
                                     (and result-type
                                          (list "result"))))))
        (format nil "~a {~%~a~%}~%"
                header
                (format nil "    if (!fatal_sbcl_error_occurred && !~a(JMP_BUF_CAST fatal_lisp_error_handler)) {
        ~a
    } else {
        ~a
    }"
                        *setjmp-operator*
                        call-statement
                        ;; If the error map does not have specify a
                        ;; fatal error code, then drop into LDB.
                        (if (and error-map (error-map-fatal-code error-map))
                            (format nil "return ~d;" (error-map-fatal-code error-map))
                            (format nil "ldb_monitor();"))))))))

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
             ,(callable-name-with-c-prefix callable-name "_")
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
           (sb-alien::initialize-alien-callable-symbol ',(callable-name-with-c-prefix callable-name "_")))))))

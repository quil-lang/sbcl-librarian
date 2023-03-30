;;; Library definition.

(in-package #:sbcl-librarian/example/libcallback)

(defun call-callback (callback)
  (sb-alien:alien-funcall callback))

(sbcl-librarian::define-type :callback
  :c-type "void*"
  :alien-type (sb-alien:* (sb-alien:function (sb-alien:* sb-alien:char)))
  :python-type "c_void_p")

(define-enum-type error-type "err_t"
  ("ERR_SUCCESS" 0)
  ("ERR_FAIL" 1))
(define-error-map error-map error-type 0
  ((t (lambda (condition)
        (declare (ignore condition))
        (return-from error-map 1)))))



(define-api libcallback-api (:error-map error-map
                             :function-prefix "callback_")
    (:literal "/* types */")
  (:type error-type)
  (:literal "/* functions */")
  (:function
   (call-callback :string ((fn :callback)))))

(define-aggregate-library libcallback (:function-linkage "CALLBACKING_API")
  sbcl-librarian:handles sbcl-librarian:environment libcallback-api)


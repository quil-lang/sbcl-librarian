;;; Library definition.

(in-package #:sbcl-librarian/tests/libs/libsquare)

(define-enum-type error-type "err_t"
  ("ERR_SUCCESS" 0)
  ("ERR_FAIL" 1))
(define-error-map error-map error-type 0
  ((t (lambda (condition)
        (declare (ignore condition))
        (return-from error-map 1)))))

(define-api libsquare-api (:error-map error-map
                         :function-prefix "square_")
  (:literal "/* types */")
  (:type error-type)
  (:literal "/* functions */")
  (:function
   (square :int ((value :int)))))

(define-aggregate-library libsquare (:function-linkage "DUMMY_API")
  sbcl-librarian:handles sbcl-librarian:environment libsquare-api)


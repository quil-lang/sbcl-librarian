;;;; bindings.lisp -- library definitions

(in-package #:sbcl-librarian.tests.consplosion)

(define-handle-type consbomb-type "consbomb_type")

(define-enum-type error-type "err_t"
  ("ERR_SUCCESS" 0)
  ("ERR_FAIL" 1))

(define-error-map error-map error-type 0
  ((t (lambda (condition)
        (declare (ignore condition))
        (return-from error-map 1)))))

(define-api consplosion-api
    (:error-map error-map)
  (:literal "/* types */")
  (:type error-type consbomb-type)
  (:literal "/* functions */")
  (:function
   (consplode consbomb-type ((n :int)))))

(define-aggregate-library consplosion (:function-linkage "DUMMY_API") ; what's this?
  sbcl-librarian:handles sbcl-librarian:environment consplosion-api)

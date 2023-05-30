;;;; Library definition

(in-package #:{{LIBRARY}})

(define-enum-type error-type "err_t"
  ("ERR_SUCCESS" 0)
  ("ERR_FAIL" 1))

(define-error-map error-map error-type 0
  ((t (lambda (condition)
        (declare (ignore condition))
        (return-from error-map 1)))))

(define-api {{LIBRARY}}-api (:error-map error-map
                             :function-prefix "callback_")
  (:literal "/* types */")
  (:type error-type)
  (:literal "/* functions */")
  (:function
   ;; put function bindings here, see sbcl-librarian/examples 
   ))

(define-aggregate-library {{LIBRARY}} (:function-linkage (string-upcase "{{CLIBRARY}}_API"))
  sbcl-librarian:handles sbcl-librarian:environment {{LIBRARY}}-api)

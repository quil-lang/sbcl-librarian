(require '#:asdf)

(asdf:load-system '#:sbcl-librarian)

(defpackage #:sbcl-librarian-example
  (:use #:cl #:sbcl-librarian))

(in-package #:sbcl-librarian-example)

(define-enum-type error-type "berr_t"
  ("BERR_SUCCESS" 0)
  ("BERR_FAIL" 1))
(define-error-map error-map error-type 0
  (t (condition) (declare (ignore condition)) 1))

(defun mantra ()
  (print "This is some add-on functionality!"))

(define-library abacus (:error-map error-map
                        :function-linkage "ABACUS_API"
                        :function-prefix "abacus_")
  (:literal "/* types */")
  (:type error-type)
  (:literal "/* functions */")
  (:function
   (mantra :void ())))

;;; Do this only at build script time.
(unless *initialize-callables-p*
  (build-bindings abacus ".")
  (build-python-bindings abacus "."))

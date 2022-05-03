(defpackage #:sbcl-librarian/benchmark
  (:use #:cl #:sbcl-librarian))

(in-package #:sbcl-librarian/benchmark)

;; An empty function.
(defun empty-function () 0)

;; An empty function returning NIL, meant for measuring the type
;; translation overhead as well as the overhead of handles.
(defun nil-function () nil)

(defun string-function (string) (cons string nil))

;; Testing overhead of identity function.
(defun identity-function (x) (x))

;; Expose a generic lisp type.
(define-handle-type lisp-type "lisp_type")

(define-enum-type error-type "err_t"
  ("ERR_SUCCESS" 0)
  ("ERR_FAIL" 1))

(define-error-map error-map error-type 0
  ((t (lambda (condition)
        (declare (ignore condition))
        (return-from error-map 1)))))

(define-api benchmark-api (:error-map error-map
                           :function-prefix "benchmark_")
  (:literal "/* types */")
  (:type lisp-type error-type)
  (:literal "/* functions */")
  (:function
   (empty-function :void ())
   (nil-function lisp-type ())
   (string-function lisp-type ((string :string)))
   (identity-function lisp-type ((x lisp-type)))))

(define-aggregate-library benchmark (:function-linkage "BENCHMARK_API")
  sbcl-librarian:handles sbcl-librarian:environment benchmark-api)

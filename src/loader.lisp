;;;; This file exposes the interface to the Lisp loader, so that we
;;;; can load more Lisp at runtime if desired.

(in-package #:sbcl-librarian)

(defun lisp-load (pathname)
  (let ((*initialize-callables-p* t))
    (load pathname)))

(define-api loader (:function-prefix "")
  (:function
   (("lisp_load" lisp-load) :void ((pathname :string)))))

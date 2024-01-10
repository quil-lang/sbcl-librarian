(require "uiop")

(define-alien-callable square int ((x int))
  (* x x))

(let ((runtime-path (first (uiop:command-line-arguments)))
      (shared-lib-suffix (second (uiop:command-line-arguments))))
  (setf (extern-alien "sbcl_runtime" (* t)) (make-alien-string runtime-path))
  (trace sb-alien::initialize-alien-callable-symbol)
  (save-lisp-and-die (concatenate 'string "libsbcl" shared-lib-suffix) :executable t :callable-exports '(square)))

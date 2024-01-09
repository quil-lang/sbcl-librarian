(require "uiop")

(let ((runtime-path (first (uiop:command-line-arguments)))
      (shared-lib-suffix (second (uiop:command-line-arguments))))
  (setf (extern-alien "sbcl_runtime" (* t)) (make-alien-string runtime-path))
  (save-lisp-and-die (concatenate 'string "libsbcl" shared-lib-suffix) :executable t))

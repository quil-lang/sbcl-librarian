(require "asdf")
(require "uiop")

(asdf:load-system :sbcl-librarian)
(asdf:register-immutable-system :sbcl-librarian)

(let ((runtime-path (first (uiop:command-line-arguments)))
      (lib-filename (second (uiop:command-line-arguments))))
  (setf (sb-alien:extern-alien "sbcl_runtime" (* t)) (sb-alien:make-alien-string runtime-path))
  (trace sb-alien::initialize-alien-callable-symbol)
  (sbcl-librarian:build-core-and-die sbcl-librarian::libsbcl "."
				     :core-name lib-filename
				     :executable t))

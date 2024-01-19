(require "asdf")
(require "uiop")

(asdf:load-system :sbcl-librarian)
(asdf:register-immutable-system :sbcl-librarian)

(let ((runtime-path (first (uiop:command-line-arguments)))
      (lib-filename (second (uiop:command-line-arguments))))
  (setf (extern-alien "sbcl_runtime" (* t)) (make-alien-string runtime-path))
  (trace sb-alien::initialize-alien-callable-symbol)
  (save-lisp-and-die lib-filename
                     :executable t
                     :callable-exports '(funcall0-by-name set-argv load-array-as-system load-shared-object)))

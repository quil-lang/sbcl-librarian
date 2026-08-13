(require 'uiop)
(require 'asdf)

(when (uiop:getenv "GITHUB_ACTIONS")
  (push :github-ci *features*))

(asdf:load-system "libcalc")

(in-package #:sbcl-librarian/example/libcalc)

(create-fasl-library-cmake-project "libcalc" calc "./libcalc/"
                                   :preload-eval-expr "(format t \"hello from preload~%\")")
(build-python-bindings calc "." :omit-init-call t)

(when (uiop:getenv "GITHUB_ACTIONS")
  (push :github-ci *features*))

(ql:quickload :libcalc)

(in-package #:sbcl-librarian/example/libcalc)

(sbcl-librarian:create-fasl-library-cmake-project "libcalc" calc "./libcalc/")
(sbcl-librarian:build-python-bindings calc "." :omit-init-call t)

(require '#:asdf)

(asdf:load-system '#:libcalc)

(when (uiop:getenv "CI")
  (push :github-ci *features*))

(in-package #:sbcl-librarian/example/libcalc)

(let ((sbcl-librarian::*non-static-lossage-handler* t))
  (build-bindings libcalc ".")
  (build-python-bindings libcalc "." #+github-ci :library-path
                                     #+(and github-ci win32) (concatenate 'string (uiop:getenv "MSYSTEM_PREFIX") "/bin/libcalc.dll")
                                     #+(and github-ci linux) "/usr/local/lib/libcalc.so"
                                     #+(and github-ci darwin) nil)
  (build-core-and-die libcalc "." :compression nil))

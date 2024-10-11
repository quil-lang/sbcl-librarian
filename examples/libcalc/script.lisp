(ql:quickload '#:libcalc)

(when (uiop:getenv "CI")
  (push :github-ci *features*))

(in-package #:sbcl-librarian/example/libcalc)

(sbcl-librarian:build-bindings libcalc ".")
(sbcl-librarian:build-python-bindings libcalc "." #+github-ci :library-path
                                   #+(and github-ci win32) (concatenate 'string (uiop:getenv "MSYSTEM_PREFIX") "/bin/libcalc.dll")
                                   #+(and github-ci linux) "/usr/local/lib/libcalc.so"
                                   #+(and github-ci darwin) nil)
(sbcl-librarian:build-core-and-die libcalc "." :compression nil))

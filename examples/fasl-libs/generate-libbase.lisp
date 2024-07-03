(push (uiop:pathname-parent-directory-pathname (uiop:pathname-parent-directory-pathname (uiop:getcwd))) ql:*local-project-directories*)

(ql:quickload :libbase)
(ql:quickload :sbcl-librarian)

(ensure-directories-exist "build/libbase/")
(sbcl-librarian:build-bindings libbase:libbase "build/libbase/")
(sbcl-librarian:build-core-and-die libbase:libbase "build/libbase/")
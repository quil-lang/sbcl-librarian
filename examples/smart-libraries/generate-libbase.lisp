(push (uiop:pathname-parent-directory-pathname (uiop:getcwd)) ql:*local-project-directories*)

(asdf:load-system :libbase)
(asdf:load-system :sbcl-librarian)

(sbcl-librarian:build-bindings libbase:libbase "build/libbase/")
(sbcl-librarian:build-core-and-die libbase:libbase "build/libbase/")
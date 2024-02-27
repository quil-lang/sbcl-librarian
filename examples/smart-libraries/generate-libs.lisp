(push (uiop:pathname-parent-directory-pathname (uiop:getcwd)) ql:*local-project-directories*)

(asdf:load-system :sbcl-librarian)
(asdf:load-system :libminiquilc)
(asdf:load-system :libminiqvm)

(sbcl-librarian:create-fasl-library-cmake-project "libminiquilc" libminiquilc:libminiquilc "build/libminiquilc" :base-library-name "libbase")
(sbcl-librarian:create-fasl-library-cmake-project "libminiqvm" libminiqvm:libminiqvm "build/libminiqvm" :base-library-name "libbase")

(uiop:quit)
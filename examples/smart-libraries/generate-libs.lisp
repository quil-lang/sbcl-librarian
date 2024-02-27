(push (uiop:pathname-parent-directory-pathname (uiop:pathname-parent-directory-pathname (uiop:getcwd))) ql:*local-project-directories*)

(ql:quickload :sbcl-librarian)
(ql:quickload :libminiquilc)
(ql:quickload :libminiqvm)

(sbcl-librarian:create-fasl-library-cmake-project "libminiquilc" libminiquilc:libminiquilc "build/libminiquilc" :base-library-name "libbase")
(sbcl-librarian:create-fasl-library-cmake-project "libminiqvm" libminiqvm:libminiqvm "build/libminiqvm" :base-library-name "libbase")

(uiop:quit)
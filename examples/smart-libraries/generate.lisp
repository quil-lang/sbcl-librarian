(require "asdf")

(asdf:load-system :sbcl-librarian)
(asdf:load-system :libminiquilc)
(asdf:load-system :libminiqvm)

(sbcl-librarian:create-fasl-library-cmake-project "libminiquilc" libminiquilc:libminiquilc "build/libquilc")
(sbcl-librarian:create-fasl-library-cmake-project "libminiqvm" libminiqvm:libminiqvm "build/libqvm")

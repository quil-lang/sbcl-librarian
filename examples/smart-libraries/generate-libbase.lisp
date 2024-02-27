(asdf:load-system :libbase)

(sbcl-librarian:build-bindings libbase:libbase "build/libbase/")
(sbcl-librarian:build-core-and-die libbase:libbase "build/libbase/")
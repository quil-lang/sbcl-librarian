(require "asdf")

(asdf:load-system :libbase)

(sbcl-librarian:build-bindings libbase:libbase ".")
(sbcl-librarian:build-core-and-die libbase:libbase ".")

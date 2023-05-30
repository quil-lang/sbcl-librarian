(require '#:asdf)

(asdf:load-system "{{LIBRARY}}")

(in-package #:{{LIBRARY}})

(build-bindings {{LIBRARY}} ".")
(build-python-bindings {{LIBRARY}} ".")
(build-core-and-die {{LIBRARY}} "." :compression t)

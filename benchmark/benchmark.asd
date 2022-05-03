(asdf:defsystem #:benchmark
  :description "Benchmark tool."
  :author "Charles Zhang <czhang@hrl.com>"
  :defsystem-depends-on (#:sbcl-librarian)
  :depends-on (#:sbcl-librarian)
  :serial t
  :components ((:file "benchmark")))

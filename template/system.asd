(asdf:defsystem #:{{LIBRARY}}
  :description "{{DESCRIPTION}}"
  :author "{{AUTHOR}}"
  :defsystem-depends-on (#:sbcl-librarian)
  :depends-on (#:sbcl-librarian #:{{SYSTEM}})
  :serial t
  :components ((:file "package")
               (:file "bindings")))

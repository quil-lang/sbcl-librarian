(defpackage #:sbcl-librarian/example/libcalc
  (:shadowing-import-from
   #:sbcl-librarian "ERROR" "WARNING" "ASSERT")
  (:use #:cl #:sbcl-librarian))

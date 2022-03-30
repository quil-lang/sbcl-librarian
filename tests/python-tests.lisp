(in-package #:sbcl-librarian.tests)

(deftest test-python ()
  "Run Python tests using pytest."
  (uiop:with-current-directory ((asdf:system-relative-pathname "sbcl-librarian" "tests/python/"))
    (terpri)
    (multiple-value-bind (output error-output exit-code)
        (uiop:run-program `(,(find-executable-pathname "pytest"))
                          :output t
                          :error-output t
                          :env `((:DYLD_LIBRARY_PATH . ,(format nil "~a:~a/src/runtime"
                                                                (test-lib-dirs-as-search-path)
                                                                (sb-ext:native-namestring (uiop:getenv "SBCL_SRC"))))
                                 (:PYTHONPATH . ,(test-lib-dirs-as-search-path))))
      (declare (ignore output error-output))
      (is (eql exit-code 0)))))

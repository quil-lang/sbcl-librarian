(in-package #:sbcl-librarian.tests)

(deftest test-build-libs ()
  (do-test-libs (lib-name lib-dir)
    (multiple-value-bind (output error-output exit-code)
        (uiop:run-program `(,(find-executable-pathname "make") "-C" ,(namestring lib-dir) "-f" "../Makefile")
                          :env `((:LIB . ,lib-name)
                                 (:SBCL_SRC . ,(sb-ext:native-namestring (uiop:getenv-pathname "SBCL_SRC")))))
      (declare (ignore output error-output))
      (is (eql exit-code 0)))))

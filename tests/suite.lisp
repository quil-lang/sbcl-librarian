(in-package #:sbcl-librarian.tests)

(defmacro do-test-libs ((name-var dir-var) &body body)
  `(uiop:with-current-directory ((asdf:system-relative-pathname "sbcl-librarian" "tests/libs/"))
     (dolist (,dir-var (uiop:subdirectories (uiop:getcwd)))
       (let ((,name-var (first (last (pathname-directory ,dir-var)))))
         ,@body))))

(defun test-lib-dirs ()
  (let ((dirs '()))
    (do-test-libs (lib-name lib-dir)
      (declare (ignore lib-name))
      (push lib-dir dirs))
    dirs))

(defun test-lib-dirs-as-search-path ()
  (format nil "~{~a:~}" (test-lib-dirs)))

(defun clean-up ()
  (do-test-libs (lib-name lib-dir)
    (uiop:run-program `("make" "clean" "-C" ,(namestring lib-dir) "-f" "../Makefile")
                      :env `((:LIB . ,lib-name)))))

(defun find-executable-pathname (name)
  (uiop:find-preferred-file
   (mapcar #'(lambda (dir)
               (sb-ext:native-namestring
                (uiop:merge-pathnames* name (uiop:ensure-pathname dir :ensure-directory t))))
           (uiop:getenv-pathnames "PATH"))))

(defun run-test-suite ()
  (run-package-tests :package '#:sbcl-librarian.tests)
  (clean-up))

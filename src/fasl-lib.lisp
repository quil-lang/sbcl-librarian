(in-package #:sbcl-librarian)

(defun create-shared-library-cmake-project (system-name package-name library-name directory)
  (let* ((build-directory (uiop:ensure-pathname directory :ensure-directory t :ensure-directories-exist t))
         (fasl-filename (concatenate 'string system-name "--all-systems.fasl"))
         (fasl-path (uiop:merge-pathnames* fasl-filename build-directory)))
    (asdf:initialize-output-translations
     `(:output-translations
       :inherit-configuration
       ((,(asdf:system-relative-pathname system-name "") #p"*--all-systems.fasl") ,build-directory)))
    (asdf:oos 'asdf:monolithic-compile-bundle-op system-name)
    (asdf:clear-output-translations)
    (load fasl-path)
    (let ((library (symbol-value (uiop:find-symbol* (string-upcase library-name) (string-upcase package-name)))))
      (build-bindings library build-directory :omit-init-function t))))

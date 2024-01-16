(in-package #:sbcl-librarian)

(defparameter *incbin-filename* "incbin.h")
(defparameter *incbin-source-text*
  #.(uiop:read-file-string
     (asdf:system-relative-pathname "sbcl-librarian"
				    (uiop:merge-pathnames* *incbin-filename* "src/"))))

(defparameter *fasl-loader-filename* "fasl_loader.c")
(defparameter *fasl-loader-constructor-name* "load_embedded_fasl")

(defparameter *cmake-minimum-required* "3.12")

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
      (build-bindings library build-directory :omit-init-function t)
      (create-incbin-source-file build-directory)
      (create-fasl-loader-source-file library fasl-filename build-directory)
      (create-cmakelists-file library fasl-filename build-directory))))

(defun create-incbin-source-file (directory)
  (with-open-file (stream (uiop:merge-pathnames* *incbin-filename* directory) :direction :output)
    (format stream *incbin-source-text*)))

(defun create-fasl-loader-source-file (library fasl-filename directory)
  (declare (ignore library))
  (with-open-file (stream (uiop:merge-pathnames* *fasl-loader-filename* directory) :direction :output)
    (format stream "#include <libsbcl.h>~%")
    (terpri stream)
    (format stream "#define INCBIN_STYLE INCBIN_STYLE_SNAKE~%")
    (format stream "#define INCBIN_PREFIX~%")
    (format stream "#include \"incbin.h\"~%")
    (terpri stream)
    (format stream "INCBIN(fasl, \"~A\");~%" fasl-filename)
    (terpri stream)
    (format stream "__attribute__((constructor))~%")
    (format stream "void ~A(void) {~%" *fasl-loader-constructor-name*)
    (format stream "    load_array(fasl_data, fasl_size);~%")
    (format stream "}")))

(defun create-cmakelists-file (library fasl-filename directory)
  (let* ((c-name (library-c-name library))
	 (bindings-filename (concatenate 'string c-name ".c"))
	 (source-filenames (list bindings-filename *incbin-filename* *fasl-loader-filename* fasl-filename)))
    (with-open-file (stream (uiop:merge-pathnames* "CMakeLists.txt" directory) :direction :output)
      (format stream "cmake_minimum_required(VERSION ~A)~%" *cmake-minimum-required*)
      (format stream "project(~A)~%" c-name)
      (format stream "configure_file(${CMAKE_CURRENT_SOURCE_DIR}/~A ${CMAKE_CURRENT_BINARY_DIR}/~A COPYONLY)~%"
	      fasl-filename fasl-filename)
      (format stream "add_library(~A SHARED ~{~A~^ ~})~%" c-name source-filenames)
      (format stream "target_link_libraries(~A PRIVATE sbcl)~%" c-name))))

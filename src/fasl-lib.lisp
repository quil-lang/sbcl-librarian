(in-package #:sbcl-librarian)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *incbin-filename* "incbin.h")
  (defparameter *incbin-source-text*
    (uiop:read-file-string
     (asdf:system-relative-pathname "sbcl-librarian"
                                    (uiop:merge-pathnames* *incbin-filename* "src/")))))

(defparameter *fasl-loader-filename* "fasl_loader.c")
(defparameter *fasl-loader-constructor-name* "load_embedded_fasls")

(defparameter *cmake-minimum-required* "3.12")

(defun create-shared-library-cmake-project (system-name package-name library-name directory)
  (let* ((build-directory (uiop:ensure-pathname directory :ensure-directory t :ensure-directories-exist t)))
    (setf (slot-value (asdf:make-operation 'asdf:prepare-bundle-op) 'asdf:sideway-operation) 'asdf:compile-bundle-op)
    (asdf:oos 'asdf:compile-bundle-op system-name)
    (let ((library (symbol-value (uiop:find-symbol* (string-upcase library-name) (string-upcase package-name))))
          (system-to-fasl-filename
            (loop :for system :in (asdf:required-components system-name
                                                            :other-systems t
                                                            :component-type 'asdf:system
                                                            :goal-operation 'asdf:compile-bundle-op)
                  :for fasl-path := (first (asdf:output-files 'asdf:compile-bundle-op system))
                  :for fasl-filename := nil
                  :when fasl-path
                    :do (setf fasl-filename (file-namestring fasl-path))
                        (uiop:copy-file fasl-path (uiop:merge-pathnames* fasl-filename build-directory))
                    :and :collect (cons system fasl-filename))))
      (build-bindings library build-directory :omit-init-function t)
      (create-incbin-source-file build-directory)
      (create-fasl-loader-source-file system-to-fasl-filename build-directory)
      (create-cmakelists-file library system-to-fasl-filename build-directory))))

(defun create-incbin-source-file (directory)
  (with-open-file (stream (uiop:merge-pathnames* *incbin-filename* directory) :direction :output)
    (format stream *incbin-source-text*)))

(defun system-c-name (system)
  (loop :with name := (asdf:component-name system)
        :for c :across "_/."
        :do (nsubstitute #\_ c name)
        :finally (return name)))

(defun create-fasl-loader-source-file (system-to-fasl-filename directory)
  (flet ((write-load-calls (stream indent-spaces)
           (loop :for (system . fasl-filename) :in system-to-fasl-filename
                 :for c-name := (system-c-name system)
                 :for data-name := (concatenate 'string c-name "_fasl_data")
                 :for size-name := (concatenate 'string c-name "_fasl_size")
                 :do (format stream "~Alisp_load_array_as_system(~A, ~A, \"~A\");~%"
                             indent-spaces data-name size-name c-name))))
    (with-open-file (stream (uiop:merge-pathnames* *fasl-loader-filename* directory) :direction :output)
      #+win32
      (format stream "#include <Windows.h>~%")
      (format stream "#include <libsbcl.h>~%")
      (terpri stream)
      (format stream "#define INCBIN_STYLE INCBIN_STYLE_SNAKE~%")
      (format stream "#define INCBIN_PREFIX~%")
      (format stream "#include \"incbin.h\"~%")
      (terpri stream)
      (loop :for (system . fasl-filename) :in system-to-fasl-filename
            :do (format stream "INCBIN(~A_fasl, \"~A\");~%" (system-c-name system) fasl-filename))
      (terpri stream)
      #-win32
      (progn
        (format stream "__attribute__((constructor))~%")
        (format stream "void ~A(void) {~%" *fasl-loader-constructor-name*)
        (write-load-calls stream "    ")
        (format stream "}"))
      #+win32
      (let ((buf-size 1024))
        (format stream "BOOL WINAPI DllMain(HINSTANCE hinstDLL, DWORD fdwReason, LPVOID lpvReserved) {~%")
        (format stream "    if (fdwReason == DLL_PROCESS_ATTACH) {~%")
        (format stream "        char buf[~D];~%" buf-size)
        (format stream "        GetModuleFileNameA(hinstDLL, buf, ~D);~%" buf-size)
        (format stream "        lisp_load_shared_object(buf);~%")
        (write-load-calls stream "        ")
        (format stream "    }~%")
        (format stream "    return TRUE;~%")
        (format stream "}")))))

(defun create-cmakelists-file (library system-to-fasl-filename directory)
  (let* ((c-name (library-c-name library))
         (bindings-filename (concatenate 'string c-name ".c"))
         (source-filenames (append (list bindings-filename *incbin-filename* *fasl-loader-filename*)
                                   (mapcar #'cdr system-to-fasl-filename))))
    (with-open-file (stream (uiop:merge-pathnames* "CMakeLists.txt" directory) :direction :output)
      (format stream "cmake_minimum_required(VERSION ~A)~%" *cmake-minimum-required*)
      (format stream "project(~A)~%" c-name)
      (loop :for (system . fasl-filename) :in system-to-fasl-filename
            :do (format stream "configure_file(${CMAKE_CURRENT_SOURCE_DIR}/~A ${CMAKE_CURRENT_BINARY_DIR}/~A COPYONLY)~%"
                        fasl-filename fasl-filename))
      (format stream "add_library(~A SHARED ~{~A~^ ~})~%" c-name source-filenames)
      (format stream "set_target_properties(~A PROPERTIES PREFIX \"\")~%" c-name)
      (format stream "target_link_libraries(~A PRIVATE sbcl)~%" c-name)
      (format stream "install(TARGETS ~A LIBRARY RUNTIME)~%" c-name)
      (format stream "install(FILES ~A.h TYPE INCLUDE)~%" c-name))))

(in-package #:sbcl-librarian)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *incbin-filename* "incbin.h"
    "The name of the file containing the incbin source code.")
  (defparameter *incbin-source-text*
    (uiop:read-file-string
     (asdf:system-relative-pathname "sbcl-librarian"
                                    (uiop:merge-pathnames* *incbin-filename* "src/")))
    "The full source code of incbin."))

(defparameter *fasl-loader-filename* "fasl_loader.c"
  "The name of the C source file that contains both embedded FASLs and a
shared library constructor that loads the embedded FASLs.")
(defparameter *fasl-loader-constructor-name* "load_embedded_fasls"
  "The name of the shared library constructor that loads embedded FASLs.")

(defparameter *cmake-minimum-required* "3.12"
  "The minimum version of CMake required to run the generated project.")

(defparameter *base-library-name* "libsbcl"
  "The name of the shared library (minus the file suffix) containing the
SBCL runtime.")

(defun create-fasl-library-cmake-project (system-name library directory &key (base-library-name "libsbcl"))
  "Generate a CMake project in DIRECTORY for a shared library that, when
loaded into a process that has already initialized the SBCL runtime,
adds the C symbols for LIBRARY to the current process's symbol table
and loads SYSTEM-NAME and its dependencies into the Lisp image while
also initializing the new C symbols.

The shared library depends on BASE-LIBRARY-NAME and contains C symbols
for the API of LIBRARY, an embedded FASL bundle file per every ASDF
system required by SYSTEM-NAME (including itself), and a shared
library constructor function that loads the embedded FASL bundle files
into the current image, using functions exported by BASE-LIBRARY-NAME,
skipping those for already-loaded systems."
  (let* ((target-system (asdf:find-system system-name))
         (build-directory (uiop:ensure-pathname (uiop:native-namestring directory)
                                                :namestring :native
                                                :defaults *default-pathname-defaults*
                                                :ensure-absolute t
                                                :ensure-directory t
                                                :ensure-directories-exist t))
         (systems (append (system-dependencies-in-load-order target-system)
                          (list target-system)))
         (*base-library-name* base-library-name))
    (compile-bundle-system-with-dependencies target-system build-directory)
    (build-bindings library build-directory :omit-init-function t :fasl-lib-p t)
    (create-incbin-source-file build-directory)
    (create-fasl-loader-source-file library systems build-directory)
    (create-cmakelists-file library systems build-directory)))

(defun create-incbin-source-file (directory)
  "Copy the incbin source code to DIRECTORY."
  (with-open-file (stream (uiop:merge-pathnames* *incbin-filename* directory) :direction :output)
    (format stream *incbin-source-text*)))

(defun system-c-name (system)
  "Replaces #\-, #\/, and #\. with #\_ in SYSTEM's name so as to produce
a valid C identifier."
  (loop :with name := (asdf:component-name system)
        :for c :across "-/."
        :do (setf name (substitute #\_ c name))
        :finally (return name)))

(defun fasl-library-load-function-name (library)
  "Returns the name of the C function for loading the embedded FASLs for LIBRARY."
  (concatenate 'string (library-c-name library) "_load"))

(defun create-fasl-loader-source-file (library systems directory)
  "Create a C source file in the DIRECTORY that embeds each of the FASL
bundle files for non-required SYSTEMS using incbin and exports a
function that requires any required SYSTEMs and then loads the
embedded FASL bundles while also initializing any alien callable
symbols defined in SYSTEMS. The C functions to perform
-requiring and loading are included from *BASE-LIBRARY-NAME*.h."
  (flet ((write-load-calls (stream indent-size)
           (loop :for system :in (remove-if-not #'system-loadable-from-fasl-p systems)
                 :for system-name := (asdf:component-name system)
                 :for c-name := (system-c-name system)
                 :for data-name := (concatenate 'string c-name "_fasl_data")
                 :for size-name := (concatenate 'string c-name "_fasl_size")
                 :do (format stream "~v@{ ~}lisp_load_array_as_system((void *) ~A, ~A, \"~A\");~%"
                             indent-size data-name size-name system-name)))
         (write-require-calls (stream indent-size)
           (loop :for system :in (remove-if-not #'require-system-p systems)
                 :for system-name := (asdf:component-name system)
                 :when (typep system 'asdf:require-system)
                   :do (format stream "~v@{ ~}lisp_require(\"~A\");~%" indent-size system-name))))
    (with-open-file (stream (uiop:merge-pathnames* *fasl-loader-filename* directory) :direction :output)
      #+win32
      (format stream "#include <Windows.h>~%")
      (format stream "#include \"~A.h\"~%" *base-library-name*)
      (terpri stream)
      (format stream "#define INCBIN_STYLE INCBIN_STYLE_SNAKE~%")
      (format stream "#define INCBIN_PREFIX~%")
      (format stream "#include \"incbin.h\"~%")
      (terpri stream)
      (loop :for system :in (remove-if-not #'system-loadable-from-fasl-p systems)
            :for c-name := (system-c-name system)
            :for fasl-filename := (system-fasl-bundle-filename system)
            :do (format stream "INCBIN(~A_fasl, \"~A\");~%" c-name fasl-filename))
      (terpri stream)
      (progn
        (let ((function-name (fasl-library-load-function-name library)))
          #+win32
          (format stream "~A~%" (library-function-linkage library))
          (format stream "void ~A(void) {~%" function-name)
          #+win32
          (let ((buf-size 1024))
            (format stream "    char dll_path[~D];~%" buf-size)
            (format stream "    HMODULE dll_mod;~%")
            (format stream "    GetModuleHandleEx(~%")
            (format stream "        GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS | GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT,~%")
            (format stream "        ~A,~%" function-name)
            (format stream "        &dll_mod);~%")
            (format stream "    GetModuleFileNameA(dll_mod, dll_path, ~D);~%" buf-size)
            (format stream "    lisp_load_shared_object(dll_path);~%")))
        (write-require-calls stream 4)
        (write-load-calls stream 4)
        (format stream "}")))))

(defun create-cmakelists-file (library systems directory)
  "Create the CMakeLists.txt file for the project in DIRECTORY. This file
contains directives for copying the FASL bundle files into the build
directory, building the shared library, and installing the shared
library and its header file."
  (let* ((c-name (library-c-name library))
         (bindings-filename (concatenate 'string c-name ".c"))
         (loadable-systems (remove-if-not #'system-loadable-from-fasl-p systems))
         (source-filenames (append (list bindings-filename *incbin-filename* *fasl-loader-filename*)
                                   (mapcar #'system-fasl-bundle-filename loadable-systems))))
    (with-open-file (stream (uiop:merge-pathnames* "CMakeLists.txt" directory) :direction :output)
      (format stream "cmake_minimum_required(VERSION ~A)~%" *cmake-minimum-required*)
      (format stream "project(~A)~%" c-name)
      (loop :for system :in loadable-systems
            :for fasl-filename := (system-fasl-bundle-filename system)
            :do (format stream "configure_file(${CMAKE_CURRENT_SOURCE_DIR}/~A ${CMAKE_CURRENT_BINARY_DIR}/~A COPYONLY)~%"
                        fasl-filename fasl-filename))
      (format stream "find_library(BASE_LIBRARY NAMES ~A${CMAKE_SHARED_LIBRARY_SUFFIX})~%" *base-library-name*)
      (format stream "add_library(~A SHARED ~{~A~^ ~}~@{ ~A~})~%" c-name source-filenames #+win32 "${BASE_LIBRARY}")
      (format stream "set_target_properties(~A PROPERTIES PREFIX \"\")~%" c-name)
      #-win32
      (format stream "target_link_libraries(~A PRIVATE ${BASE_LIBRARY})~%" c-name)
      (format stream "install(TARGETS ~A LIBRARY RUNTIME)~%" c-name)
      (format stream "install(FILES ~A.h TYPE INCLUDE)~%" c-name))))

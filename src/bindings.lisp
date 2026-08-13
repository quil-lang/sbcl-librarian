(in-package #:sbcl-librarian)

(defparameter *windows-export-linkage*
  "__declspec(dllexport)")

(defparameter *windows-import-linkage*
  "__declspec(dllimport)")

(defparameter *elf-export-linkage*
  "__attribute__ ((visibility (\"default\")))")

(defun write-linkage-macro (linkage build-name stream)
  (let ((windows "_WIN64")
        (elf "__ELF__"))
    (format stream "#if defined(~A)~%" build-name)
    (format stream "#  if defined(~A)~%" windows)
    (format stream "#    define ~A ~A~%" linkage *windows-export-linkage*)
    (format stream "#  elif defined(~A)~%" elf)
    (format stream "#    define ~A ~A~%" linkage *elf-export-linkage*)
    (format stream "#  else~%")
    (format stream "#    define ~A~%" linkage)
    (format stream "# endif~%")
    (format stream "#else~%")
    (format stream "#  if defined(~A)~%" windows)
    (format stream "#    define ~A ~A~%" linkage *windows-import-linkage*)
    (format stream "#  else~%")
    (format stream "#  define ~A~%" linkage)
    (format stream "#  endif~%")
    (format stream "#endif~%~%")))

(defun write-api-to-header (api linkage stream)
  (dolist (spec (api-specs api))
    (destructuring-bind (kind &rest things) spec
      (ecase kind
        (:literal
         (dolist (literal things)
           (format stream literal)
           (terpri stream)))
        (:type
         (dolist (type things)
           (write-line (type-definition type) stream)))
        (:function
         (dolist (spec things)
           (destructuring-bind (name result-type typed-lambda-list) spec
             (format stream "~A;~%"
                     (c-function-declaration name result-type typed-lambda-list
                                             :datap nil
                                             :linkage linkage
                                             :externp t
                                             :function-prefix (api-function-prefix api)
                                             :error-map (api-error-map api))))))))))

(defun write-api-to-source (api linkage stream)
  "For each function in `api' this function:

- Write an explicit `extern' declaration for a variable to tell linker
  to expose its symbol. This will allow `initialize_lisp' and/or `load'
  to find and fill it with a pointer to corresponding LISP
  function. `extern' declarations do not allocate space, so we need to
  declare it again without `extern'.

- Write a second declaration of this variable which allocates space and
  explicitly assigns NULL to it at compilation time so that we can
  reliably detect if it was not assigned with a pointer by
  `initialize_lisp' or `load' for some reason.

- Write a C function which properly calls corresponding LISP function
  referenced by the declared variable.

We do not place the `extern' declaration of the pointer variable into a
header file (like we do it with function's `extern' declaration) because
it is actually an implementation detail which is not intended to be
accessed explicitly by users.
"
  (dolist (spec (api-specs api))
    (destructuring-bind (kind &rest things) spec
      (ecase kind
        (:literal)
        (:type)
        (:function
         (dolist (spec things)
           (destructuring-bind (name result-type typed-lambda-list) spec
             (format stream "extern ~A;~%~:*~A = NULL;~%~A~%"
                     (c-function-declaration name result-type typed-lambda-list
                                             :datap t
                                             :externp nil
                                             :linkage linkage
                                             :function-prefix (api-function-prefix api)
                                             :c-prefix "_"
                                             :error-map (api-error-map api))
                     (c-function-definition name result-type typed-lambda-list
                                            :function-prefix (api-function-prefix api)
                                            :error-map (api-error-map api))))))))))

(defun write-init-function (name linkage stream &optional (initialize-lisp-args nil))
  (terpri stream)
  (format stream "extern int initialize_lisp(int argc, char **argv);~%~%")
  (format stream "~A {~%"
          (c-function-declaration name ':int '((core :string))
                                  :datap nil
                                  :linkage linkage))
  (format stream "  static int initialized = 0;~%")
  (format stream "  char *init_args[] = {\"\", \"--core\", core, \"--noinform\", ~{\"~a\"~^, ~}};~%"
          initialize-lisp-args)
  (format stream "  if (initialized) return 1;~%")
  (format stream "  if (initialize_lisp(~a, init_args) != 0) return -1;~%"
          (+ 4 (length initialize-lisp-args)))
  (format stream "  initialized = 1;~%")
  (format stream "  return 0; }"))

(defun build-bindings (library directory &key (omit-init-function nil)
                                              (initialize-lisp-args nil))
  (let* ((c-name (library-c-name library))
         (header-name (concatenate 'string c-name ".h"))
         (source-name (concatenate 'string c-name ".c"))
         (linkage (library-function-linkage library))
         (build-flag (and linkage
                          (concatenate 'string linkage "_BUILD"))))
    ;; header
    (with-open-file (stream (merge-pathnames header-name directory)
                            :direction :output
                            :if-exists :supersede)
      (let ((guard (format nil "_~A_h" c-name)))       
        (format stream "#ifndef ~A~%" guard)
        (format stream "#define ~A~%~%" guard))
      (when linkage
        (write-linkage-macro linkage build-flag stream))
      (format stream "#include <sbcl_librarian_err.h>~%~%")
      (dolist (api (library-apis library))
        (write-api-to-header api linkage stream))
      (unless omit-init-function
        (format stream "~A;~%~%"
                (c-function-declaration 'init ':int '((core :string))
                                        :datap nil
                                        :linkage linkage)))
      (format stream "#endif~%"))
    ;; source
    (with-open-file (stream (merge-pathnames source-name directory)
                            :direction :output
                            :if-exists :supersede)
      (format stream "#define ~A~%~%" build-flag)
      (format stream "#include ~s~%~%" header-name)
      (format stream "#include <signal.h>~%")
      (format stream "#ifndef _WIN32~%#include <pthread.h>~%#endif~%~%")
      (dolist (api (library-apis library))
        (write-api-to-source api linkage stream))
      (unless omit-init-function
        (write-init-function 'init linkage stream initialize-lisp-args)))))

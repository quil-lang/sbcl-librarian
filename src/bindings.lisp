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

(defun write-api-to-source (api stream)
  (dolist (spec (api-specs api))
    (destructuring-bind (kind &rest things) spec
      (ecase kind
        (:literal)
        (:type)
        (:function
         (dolist (spec things)
           (destructuring-bind (name result-type typed-lambda-list) spec
             (format stream "~A;~%~A~%"
                     (c-function-declaration name result-type typed-lambda-list
                                             :datap t
                                             :externp nil
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
  ;; Set the lossage handler to a function that longjmps out of the
  ;; most recent call-into-Lisp
  (when (sb-sys:find-foreign-symbol-address "set_lossage_handler")
    (format stream "  set_lossage_handler(return_from_lisp);~%"))
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
      #-win32
      (format stream "#include <setjmp.h>~%")
      #+win32
      (format stream "#include <stdint.h>~%")
      (format stream "#include <stdio.h>~%~%")
      ;; On Windows we use __builtin_setjmp and __builtin_longjmp (see
      ;; the documentation for *longjmp-operator* for rationale) which
      ;; use a jump buffer of type intptr_t[5] instead of jmp_buf (see
      ;; https://gcc.gnu.org/onlinedocs/gcc/Nonlocal-Gotos.html).
      #-win32
      (format stream "__thread jmp_buf fatal_lisp_error_handler;~%")
      #+win32
      (format stream "__thread intptr_t fatal_lisp_error_handler[5];~%")
      ;; Clang's __builtin_{set,long}jmp are compatibile with the GCC
      ;; equivalents[^1]. However, Clang types the jmp_buf arguments
      ;; to these builtins as `(void **)` instead of `intptr[5]`[^2],
      ;; so we add a macro to conditionally insert the cast.
      ;;
      ;; [^1]: https://github.com/llvm/llvm-project/blob/ceade83ad5fc529f2b2beb896eec0dd0b29fdd44/llvm/docs/ExceptionHandling.rst#id32
      ;; [^2]: https://github.com/llvm/llvm-project/blob/ceade83ad5fc529f2b2beb896eec0dd0b29fdd44/clang/include/clang/Basic/Builtins.td#L897
      (format stream "#ifdef __clang__~%# define JMP_BUF_CAST (void **)~%#else~%# define JMP_BUF_CAST~%#endif~%~%")
      (when (sb-sys:find-foreign-symbol-address "set_lossage_handler")
        (format stream "void set_lossage_handler(void (*handler)(void));~%"))
      (format stream "void ldb_monitor(void);~%~%")
      (format stream "int fatal_sbcl_error_occurred = 0;~%~%")
      (format stream "void return_from_lisp(void) { fatal_sbcl_error_occurred = 1; fflush(stdout); fflush(stderr); ~a(JMP_BUF_CAST fatal_lisp_error_handler, 1); }~%~%"
              *longjmp-operator*)
      (dolist (api (library-apis library))
        (write-api-to-source api stream))
      (unless omit-init-function
        (write-init-function 'init linkage stream initialize-lisp-args)))))

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
                                             :datap t
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
             (format stream "~A;~%"
                     (c-function-declaration name result-type typed-lambda-list
                                             :datap t
                                             :externp nil
                                             :function-prefix (api-function-prefix api)
                                             :error-map (api-error-map api))))))))))

(defun write-init-function (name linkage stream &optional (initialize-lisp-args nil)
                                                          (constructor-p nil))
  (terpri stream)
  (format stream "extern int initialize_lisp(int argc, char **argv);~%~%")
  (when constructor-p
    (format stream "__attribute__((constructor))~%"))
  (format stream "~A {~%"
          (c-function-declaration name ':int (and (not constructor-p) '((core :string)))
                                  :datap nil
                                  :linkage linkage))
  (when constructor-p
    #-win32
    (format stream "  Dl_info info; dladdr(~A, &info); char *core = info.dli_fname;~%" name)
    #+win32
    (let ((buf-size 1024))
      (format stream "  HMODULE dll_mod; char dll_path[~D];~%" buf-size)
      (format stream "  GetModuleHandleEx(~%")
      (format stream "    GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS | GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT,~%")
      (format stream "    ~A,~%" name)
      (format stream "    &dll_mod);")
      (format stream "  GetModuleFileNameA(dll_mod, dll_path, ~D);~%" buf-size)
      (format stream "  char *core = dll_path;")))
  (format stream "  static int initialized = 0;~%")
  (format stream "  char *init_args[] = {\"\", \"--core\", core, \"--noinform\", ~{\"~a\"~^, ~}};~%"
          initialize-lisp-args)
  (format stream "  if (initialized) return 1;~%")
  (format stream "  if (initialize_lisp(~a, init_args) != 0) return -1;~%"
          (+ 4 (length initialize-lisp-args)))
  (format stream "  initialized = 1;~%")
  (format stream "  return 0; }"))

(defun build-bindings (library directory &key (omit-init-function nil)
                                              (initialize-lisp-args nil)
                                              (init-is-constructor-p nil))
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
      (when init-is-constructor-p
        #-win32
        (format stream "#include <dlfcn.h>~%")
        #+win32
        (format stream "~@{#include<~A>~%~}" "Windows" "Process" "psapi.h")
        (terpri stream))
      (format stream "#define ~A~%~%" build-flag)
      (format stream "#include ~s~%~%" header-name)
      (dolist (api (library-apis library))
        (write-api-to-source api stream))
      (unless omit-init-function
        (write-init-function 'init linkage stream initialize-lisp-args init-is-constructor-p)))))

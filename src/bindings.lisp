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
  (labels ((fdecl (name result-type typed-lambda-list &optional (datap t))
             (c-function-declaration name result-type typed-lambda-list
                                     :datap datap
                                     :linkage linkage
                                     :function-prefix (api-function-prefix api)
                                     :error-map (api-error-map api)))
           (write-spec (spec)
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
                            (fdecl name result-type typed-lambda-list)))))))))
    (map nil #'write-spec (api-specs api))))

(defun write-init-function (name linkage stream)
  (terpri stream)
  (format stream "extern int initialize_lisp(int argc, char **argv);~%~%")
  (format stream "~A {~%"
          (c-function-declaration name ':int '((core :string))
                                  :datap nil
                                  :linkage linkage))
  (format stream "  static int initialized = 0;~%")
  (format stream "  char *init_args[] = {\"\", \"--core\", core, \"--noinform\"};~%")
  (format stream "  if (initialized) return 1;~%")
  (format stream "  if (initialize_lisp(4, init_args) != 0) return -1;~%")
  (format stream "  initialized = 1;~%")
  (format stream "  return 0; }"))

(defun build-bindings (library directory)
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
      (format stream "~A;~%~%"
              (c-function-declaration 'init ':int '((core :string))
                                      :datap nil
                                      :linkage linkage))
      (format stream "#endif~%"))
    ;; source
    (with-open-file (stream (merge-pathnames source-name directory)
                            :direction :output
                            :if-exists :supersede)
      (format stream "#define ~A~%~%" build-flag)
      (format stream "#include ~s~%~%" header-name)
      (write-init-function 'init linkage stream))))

(in-package #:sbcl-librarian)

(defparameter *windows-export-linkage*
  "__declspec(dllexport)")

(defparameter *windows-import-linkage*
  "__declspec(dllimport)")

(defparameter *elf-export-linkage*
  "__attribute__ ((visibility (\"default\")))")

(defun write-linkage-macro (linkage build-name stream)
  (let ((windows "_WIN32")
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
    (format stream "#    define ~A~%" linkage)
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
                                              (initialize-lisp-args nil)
                                              (fasl-lib-p nil))
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
      (when fasl-lib-p
        (format stream "~A;~%~%"
                (c-function-declaration (fasl-library-load-function-name library)':void '()
                                        :datap nil
                                        :linkage linkage)))
      (format stream "#endif~%"))
    ;; source
    (with-open-file (stream (merge-pathnames source-name directory)
                            :direction :output
                            :if-exists :supersede)
      (format stream "#define ~A~%~%" build-flag)
      (format stream "#include ~s~%~%" header-name)
      (dolist (api (library-apis library))
        (write-api-to-source api stream))
      (unless omit-init-function
        (write-init-function 'init linkage stream initialize-lisp-args)))))

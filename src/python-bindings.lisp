(in-package #:sbcl-librarian)

(defun python-function-definition (name result-type typed-lambda-list &key (datap t)
                                                                        function-prefix
                                                                        error-map
                                                                        library-name)
  (declare (ignore datap))              ; TODO: is this right?
  (multiple-value-bind (callable-name return-type typed-lambda-list result-type)
      (canonical-signature name result-type typed-lambda-list
                           :function-prefix function-prefix
                           :error-map error-map)
    (format nil
            "~a = ~a.~a
~a.restype = ~a
~a.argtypes = [~{~a~^, ~}]
~:[~;~a = sbcl_librarian.wrapper.lift_fn(\"~:*~a\", ~:*~a)~%~]"
            ;; First line
            (coerce-to-c-name callable-name)
	    library-name
	    (coerce-to-c-name callable-name)
            ;; Second line
            (coerce-to-c-name callable-name)
            (python-type return-type)
            ;; Third line
            (coerce-to-c-name callable-name)
            (append
             (loop :for (name type) :in typed-lambda-list
                   :collect (python-type type))
             (and result-type
                  (list (format nil "POINTER(~a)" (python-type result-type)))))
	    ;; Fourth line (optional)
	    (eql 'error-type return-type)
	    (coerce-to-c-name callable-name))))

(defun write-default-python-header (library stream &optional (omit-init-call nil)
                                                             (library-path nil))
  (let ((name (library-c-name library)))
    (format stream "import os~%")
    (format stream "from ctypes import *~%")
    (format stream "from ctypes.util import find_library~%")
    (format stream "from pathlib import Path~%~%")
    (format stream "import sbcl_librarian.wrapper~%")
    (format stream "from sbcl_librarian.errors import lisp_err_t~%~%")

    (if library-path
        (format stream "libpath = Path(\"~a\")~%~%" library-path)
        (progn
          (format stream "try:~%")
          (format stream "    libpath = Path(find_library('~a')).resolve()~%" (subseq name 3))
          (format stream "except TypeError as e:~%")
          (format stream "    raise Exception('Unable to locate ~a') from e~%~%" name)))

    (format stream "~a = CDLL(str(libpath), mode=RTLD_GLOBAL)~%~%" name)
    (unless omit-init-call
      (format stream "~a.init(str(libpath.parent / '~a.core').encode('utf-8'))~%~%"
              name name))))

(defun write-api-to-python (api library-name stream)
  (loop :for (kind . things) :in (api-specs api)
        :append (ecase kind
                  (:literal nil)
                  (:type
                   (loop :for type :in things
                         :do (format stream "~A~%" (python-type-definition type))
                         :collect (c-type type)))
                  (:function
                   (loop :for (name result-type typed-lambda-list) :in things
                         :do (format stream "~A~%"
                                     (python-function-definition
                                      name result-type typed-lambda-list
                                      :function-prefix (api-function-prefix api)
                                      :error-map (api-error-map api)
                                      :library-name library-name))
                         :collect (coerce-to-c-name (prefix-name (api-function-prefix api) name)))))))

(defun build-python-bindings (library directory &key (omit-init-call nil) (text-between-header-and-exports "")
                                                     (library-path nil) (write-python-header-fn #'write-default-python-header))
  "Generates a Python source file in DIRECTORY containing bindings for
LIBRARY. The default consists of a header, which loads the shared
library and calls its init function, followed by a sequence of
bindings, which create wrappers for the functions exported by the
shared library.

If OMIT-INIT-CALL is T, then don't call the init function after
loading the shared library.

TEXT-BETWEEN-HEADER-AND-EXPORTS is inserted verbatim after the header
and before the bindings.

If LIBRARY-PATH is not NIL, then load the shared library directly from
the provided path instead of using ctypes.util.find_library.

If WRITE-PYTHON-HEADER-FN is provided, then call that function instead
of WRITE-DEFAULT-PYTHON-HEADER to generate the header text. The
provided function must have the same signature as
WRITE-DEFAULT-PYTHON-HEADER."
  (let ((file-name (concatenate 'string (library-c-name library) ".py")))    
    (with-open-file (stream (merge-pathnames file-name directory)
                            :direction :output
                            :if-exists :supersede)
      (funcall write-python-header-fn library stream omit-init-call library-path)
      (write-string text-between-header-and-exports stream)
      (terpri stream)
      (let* ((api-exports 
               (loop :for api :in (library-apis library)
                     :append (write-api-to-python api (library-c-name library) stream))))
        (format stream "~%~%__all__ = [~{'~a'~^, ~}]~%~%"
                api-exports)))))

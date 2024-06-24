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
            "~a = CFUNCTYPE(~a, ~{~a~^, ~})(c_void_p.in_dll(~a, '~a').value)"
            (coerce-to-c-name callable-name)
            (python-type return-type)
            (append
             (loop :for (name type) :in typed-lambda-list
                   :collect (python-type type))
             (and result-type
                  (list (format nil "POINTER(~a)" (python-type result-type)))))
            library-name
            (coerce-to-c-name callable-name))))

(defun write-default-python-header (library stream &optional (omit-init-call nil)
                                                             (library-path nil))
  (let ((name (library-c-name library)))
    (format stream "import os~%")
    (format stream "from ctypes import *~%")
    (format stream "from ctypes.util import find_library~%")
    (format stream "from pathlib import Path~%~%")

    (if library-path
        (format stream "libpath = Path(\"~a\")~%~%" library-path)
        (progn
          (format stream "try:~%")
          (format stream "    libpath = Path(find_library('~a')).resolve()~%" name)
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
                                                     (library-path nil))
  (let ((file-name (concatenate 'string (library-c-name library) ".py")))    
    (with-open-file (stream (merge-pathnames file-name directory)
                            :direction :output
                            :if-exists :supersede)
      (funcall 'write-default-python-header library stream omit-init-call library-path)
      (write-string text-between-header-and-exports stream)
      (terpri stream)
      (let* ((api-exports 
               (loop :for api :in (library-apis library)
                     :append (write-api-to-python api (library-c-name library) stream))))
        (format stream "~%~%__all__ = [~{'~a'~^, ~}]~%~%"
                api-exports)))))

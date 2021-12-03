(in-package #:sbcl-librarian)

(defclass library ()
  ((name :initarg :name
         :accessor library-name)
   (function-linkage :initarg :function-linkage
                     :accessor library-function-linkage)
   (apis :initarg :apis
         :accessor library-apis)))

(defparameter *standard-boilerplate*
  `((:function
      (release-handle :void ((handle :pointer)))
      (handle-eq :bool ((a :pointer) (b :pointer)))))
  "Standard routines for use with user-defined APIs.")

(defmacro define-library (name (&key
                                  error-map
                                  (function-prefix "")
                                  (function-linkage
                                   (concatenate 'string
                                                (string-upcase function-prefix)
                                                "API")))
                          &body specs)
  (let ((library-api (gensym "API"))
        (standard-boilerplate (gensym "API")))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (let ((,standard-boilerplate (make-instance 'api
                                      :name 'standard-boilerplate
                                      :error-map nil
                                      :function-prefix ,function-prefix
                                      :specs ',*standard-boilerplate*))
             (,library-api (make-instance 'api
                             :name ',name
                             :error-map ',error-map
                             :function-prefix ,function-prefix
                             :specs ',specs)))
         ,@(callable-definitions-from-spec function-prefix nil *standard-boilerplate*)
         ,@(callable-definitions-from-spec function-prefix error-map specs)
         (define-aggregate-library ,name
             (:function-linkage ,function-linkage)
           ,library-api
           ,standard-boilerplate)))))

(defmacro define-aggregate-library (name (&key function-linkage) &body apis)
  `(defvar ,name
     (make-instance 'library
       :name ',name
       :function-linkage ,function-linkage
       :apis (list ,@apis))))

;;; core stuff

(defun library-c-name (library)
  (lisp-to-c-name (library-name library)))

(defgeneric callable-exports (obj)
  (:method ((library library))
    (loop :for api :in (library-apis library)
          :append (callable-exports api)))
  (:method ((api api))
    (loop :for (kind . things) :in (api-specs api)
          :when (eq kind ':function)
            :append (mapcar (lambda (spec)
                              (intern (exported-name api (first spec) :lisp t)
                                      (symbol-package (first spec))))
                            things))))

(defun build-core-and-die (library directory)
  (let* ((c-name (library-c-name library))
         (core-name (concatenate 'string c-name ".core")))
    (sb-ext:save-lisp-and-die
     (merge-pathnames core-name directory)
     :callable-exports (callable-exports library))))

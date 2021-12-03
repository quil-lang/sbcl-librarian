(in-package #:sbcl-librarian)

;; mirrors handler case

(defgeneric error-map-type (error-map))

(defmacro define-error-map (name error-type no-error &body cases)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defmethod wrap-error-handling (form (error-map (eql ',name)))
       `(handler-case (progn ,form ,,no-error)
          ,',@cases))
     (defmethod error-map-type ((error-map (eql ',name)))
       ',error-type)
     (defmethod error-map-success-code ((error-map (eql ',name)))
       ,no-error)))

(defun c-to-lisp-name (c-name)
  (nsubstitute #\- #\_ (string-upcase c-name)))

(defun lisp-to-c-name (lisp-name)
  (nsubstitute #\_ #\- (string-downcase (symbol-name lisp-name))))

(defun callable-definitions-from-spec (function-prefix error-map specs)
  (nreverse
   (loop :for (kind . things) :in specs
         :when (eq kind ':function)
           :append (loop :for (name result-type typed-lambda-list) :in things
                         :collect (callable-definition
                                   name result-type typed-lambda-list
                                   :function-prefix function-prefix
                                   :error-map error-map)))))


(defclass api ()
  ((name :initarg :name
         :accessor api-name)
   (error-map :initarg :error-map
              :accessor api-error-map)
   (function-prefix :initarg :function-prefix
                    :accessor api-function-prefix)
   (specs :initarg :specs
          :accessor api-specs))
  (:documentation "TODO"))

(defmacro define-api (name (&key error-map
                              (function-prefix ""))
                      &body specs)  
  `(progn
     ,@(callable-definitions-from-spec function-prefix error-map specs)
     (defvar ,name
       (make-instance 'api
         :name ',name
         :error-map ',error-map
         :function-prefix ,function-prefix
         :specs ',specs))))

(defmacro export-api (api)
  `(progn
     ,@(callable-definitions-from-spec (api-function-prefix api)
                                       (api-error-map api)
                                       (api-specs api))))

(defun exported-name (api name &key (lisp nil))
  (let ((c-name
          (concatenate 'string
                       (api-function-prefix api)
                       (lisp-to-c-name name))))
    (if lisp
        (c-to-lisp-name c-name)
        c-name)))




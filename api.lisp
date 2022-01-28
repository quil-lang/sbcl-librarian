(in-package #:sbcl-librarian)

;; mirrors handler case

(defgeneric error-map-type (error-map))

(defmacro define-error-map (name error-type no-error &body cases)
  "Define an error map with the indicated NAME. 

Error maps control how Lisp errors get translated to error codes at exported function boundaries. There are three pieces involved:
- ERROR-TYPE is the name of the error type to be returned,
- NO-ERROR is the value to return in the absence of errors,
- CASES are fragments of a HANDLER-CASE form (cf. the source).
"
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

(defun coerce-to-c-name (name)
  (typecase name
    (list (car name))
    (symbol (lisp-to-c-name name))
    (string name)))

(defun callable-definitions-from-spec (function-prefix error-map specs)
  "Generate ALIEN-CALLABLE definitions from the given SPEC.

Prepends FUNCTION-PREFIX to generated function names, and wraps error handling according to ERROR-MAP."
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
         :accessor api-name
         :documentation "The Lisp-style name of the API (a symbol).")
   (error-map :initarg :error-map
              :accessor api-error-map
              :documentation "An error map, used at the lisp<->alien boundary to translate conditions to ordinary return values.")
   (function-prefix :initarg :function-prefix
                    :accessor api-function-prefix
                    :documentation "A string prepended to all exported function names.")
   (specs :initarg :specs
          :accessor api-specs
          :documentation "A list of specifications."))
  (:documentation "A specification of functions and types for export to a shared library."))

(defmacro define-api (name (&key error-map (function-prefix ""))
                      &body specs)
  "Define an API.

In addition to constructing a suitable API object, this also ensures that alien callable definitions are defined."
  `(progn
     ,@(callable-definitions-from-spec function-prefix error-map specs)
     (defvar ,name
       (make-instance 'api
         :name ',name
         :error-map ',error-map
         :function-prefix ,function-prefix
         :specs ',specs))))

(defun prefix-name (prefix name)
  "Prefix the generalized NAME with the string PREFIX."
  (flet ((c-name (c-name)
           (concatenate 'string prefix c-name))
         (lisp-name (lisp-name)
           (intern
            (concatenate 'string
                         (c-to-lisp-name prefix)
                         (symbol-name lisp-name))
            (symbol-package lisp-name))))
    (etypecase name
      (list (list (c-name (first name))
                  (lisp-name (second name))))
      (symbol (lisp-name name))
      (string (c-name name)))))

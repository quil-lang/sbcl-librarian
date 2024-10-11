(in-package #:sbcl-librarian)

;; mirrors handler case

(defgeneric error-map-type (error-map))

(defgeneric error-map-fatal-code (error-map))

(defmacro define-error-map (name error-type (&key no-error fatal-error) bindings)
  "Define an error map with the indicated NAME. 

Error maps control how Lisp errors get translated to error codes at exported function boundaries. There are three pieces involved:
- ERROR-TYPE is the name of the error type to be returned,
- NO-ERROR is the value to return in the absence of errors,
- BINDINGS are bindings of an enclosing HANDLER-BIND form (cf. the source).

All Lisp calls will get wrapped in a block named NAME, within which a HANDLER-BIND form is present. Error handlers may (RETURN-FROM <NAME> <ERROR-CODE>) to propagate errors."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defmethod wrap-error-handling (form (error-map (eql ',name)))
       `(block ,',name
          (handler-bind
              ,',bindings
            (progn ,form ,,no-error))))
     (defmethod error-map-type ((error-map (eql ',name)))
       ',error-type)
     (defmethod error-map-success-code ((error-map (eql ',name)))
       ,no-error)
     (defmethod error-map-fatal-code ((error-map (eql ',name)))
       ,fatal-error)))

(defun c-to-lisp-name (c-name)
  (nsubstitute #\- #\_ (string-upcase c-name)))

(defun lisp-to-c-name (lisp-name)
  (nsubstitute #\_ #\- (string-downcase (symbol-name lisp-name))))

(defun callable-name-with-c-prefix (callable-name prefix)
  "Every alien callable function is associated with a callable name. A
callable name is a two-element list: the first element is a string
naming a C symbol, and the second element is a symbol naming a Lisp
function. During runtime initialization, the memory address of the
callable name's C symbol is populated with a pointer to a dynamically
generated trampoline function wrapping the callable name's Lisp
function.

A callable name can also be referred to by one of its two elements
alone, in which case the other element is implicitly defined by
substituting underscores in the string for hyphens in the symbol, or
vice versa.

This function takes a callable name in any one of its three formats
and returns a new callable name where the string naming the C symbol
is prefixed by PREFIX, leaving the symbol naming the Lisp function unchanged.

For example, if the prefix is \"_\":

(\"foo_bar\" FOO-BAR) = \"foo_bar\" = FOO-BAR
all become
(\"_foo_bar\" FOO-BAR)"
  (multiple-value-bind (c-name lisp-name)
      (etypecase callable-name
        (list (values (first callable-name) (second callable-name)))
        (symbol (values (lisp-to-c-name callable-name) callable-name))
        (string (values callable-name (c-to-lisp-name callable-name))))
    (list (concatenate 'string prefix c-name) lisp-name)))

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

(defmacro define-api (name (&key (function-prefix ""))
                      &body specs)
  "Define an API.

In addition to constructing a suitable API object, this also ensures that alien callable definitions are defined."
  `(progn
     ,@(callable-definitions-from-spec function-prefix 'default-error-map specs)
     (defvar ,name
       (make-instance 'api
         :name ',name
         :error-map 'default-error-map
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

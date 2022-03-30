(in-package #:sbcl-librarian)

(defgeneric c-type (type)
  (:documentation "The C representation of TYPE, as a string."))

(defgeneric type-definition (type)
  (:documentation "The C definition associated with TYPE, if it exists."))

(defgeneric sb-alien-type (type)
  (:documentation "The SB-ALIEN representation of TYPE."))

(defgeneric python-type (type)
  (:documentation "The Python representation of TYPE, as a string."))

(defgeneric python-type-definition (type)
  (:documentation "The Python definition associated with TYPE, if it exists."))

(defgeneric lisp-to-alien (type)
  (:documentation "A function to coerce values of type from ordinary lisp values to SB-ALIEN values.")
  (:method (type)
    #'identity))

(defgeneric alien-to-lisp (type)
  (:documentation "A function to coerce values of type from SB-ALIEN back to ordinary lisp values.")
  (:method (type)
    #'identity))

(defmacro define-type (name &key c-type
                              definition
                              python-type
                              python-type-definition
                              alien-type
                              lisp-to-alien
                              alien-to-lisp)
  "Define a new type with the provided NAME.

Keyword Arguments
 - C-TYPE: the C representation of the type, as a C source string.
 - ALIEN-TYPE: the SB-ALIEN representation of the type.
 - DEFINITION: (optional) the C source defining the type.
 - PYTHON-TYPE: (optional) the Python representation of the type, as a C source string.
 - PYTHON-TYPE-DEFINITION: (optional) the Python source defining the type.
 - LISP-TO-ALIEN: (optional) a routine to convert from ordinary Lisp values to SB-ALIEN values.
 - ALIEN-TO-LISP: (optional) a routine to convert from SB-ALIEN values to ordinary Lisp values."
  (unless (symbolp name)
    (error "Expected a symbol type name, but got ~A" name))
  (when (or (and lisp-to-alien (null alien-to-lisp))
            (and alien-to-lisp (null lisp-to-alien)))
    (warn "Partial type coercion specified; be careful!"))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defmethod c-type ((lisp-name (eql ',name)))
       ,c-type)
     (defmethod sb-alien-type ((lisp-name (eql ',name)))
       ',alien-type)
     ,@(when definition
         `((defmethod type-definition ((lisp-name (eql ',name)))
             ,definition)))
     ,@(when python-type
         `((defmethod python-type ((lisp-name (eql ',name)))
             ,python-type)))
     ,@ (when python-type-definition
          `((defmethod python-type-definition ((lisp-name (eql ',name)))
              ,python-type-definition)))
     ,@(when lisp-to-alien
         (let ((op (gensym)))
           `((let ((,op ,lisp-to-alien))
               (defmethod lisp-to-alien ((lisp-name (eql ',name)))
                 ,op)))))
     ,@(when alien-to-lisp
         (let ((op (gensym)))
           `((let ((,op ,alien-to-lisp))
               (defmethod alien-to-lisp ((lisp-name (eql ',name)))
                 ,op)))))))

;;; standard types

(define-type :int
    :c-type "int" :alien-type sb-alien:int :python-type "c_int")

(define-type :unsigned-int
    :c-type "unsigned int" :alien-type sb-alien:unsigned-int :python-type "c_unt")

(define-type :string
    :c-type "char*" :alien-type sb-alien:c-string :python-type "c_char_p")

(define-type :bool
    :c-type "int" :alien-type sb-alien:int :python-type "c_bool"
    :lisp-to-alien (lambda (c) `(if ,c 1 0))
    :alien-to-lisp (lambda (c) `(not (zerop ,c))))

(define-type :float
    :c-type "float" :alien-type sb-alien:float :python-type "c_float")

(define-type :double
    :c-type "double" :alien-type sb-alien:double :python-type "c_double")

(define-type :pointer
    :c-type "void*" :alien-type (sb-alien:* t) :python-type "c_void_p")

(define-type :void
    :c-type "void" :alien-type sb-alien:void :python-type "None")


;;; some convenience type constructors

(defmacro define-handle-type (name c-type)
  "Define a handle type named NAME, using name C-TYPE in exported code.

Handles serve roughly as opaque pointers to Lisp objects."
  `(define-type ,name
       :c-type ,c-type
       :alien-type (sb-alien:* t)
       :definition ,(format nil "struct ~a__ { int unused; }; typedef struct ~:*~a__ *~:*~a;" c-type)
       :python-type ,c-type
       :python-type-definition ,(format nil "class ~a(c_void_p):~%    pass~%~%" c-type)
       :lisp-to-alien (lambda (c) `(make-handle ,c))
       :alien-to-lisp (lambda (c) `(dereference-handle ,c))))

(defmacro define-enum-type (name c-type &rest enums)
  "Define an enum type named NAME, using name C-TYPE in exported code.

Here ENUMS contains expressions of the form (<string> <int>), indicating that the C name given by <string> corresponds to an enum value <int>."
  `(define-type ,name
       :c-type ,c-type
       :alien-type sb-alien:int
       :definition ,(format nil "typedef enum { ~:{~a = ~d, ~}} ~a;" enums c-type)
       :python-type ,c-type
       :python-type-definition ,(format nil "class ~a(int):~%    _map = {~%~{~a~%~}    }~%~%"
                                        c-type
                                        (loop :for (name code) :in enums
                                              :collect (format nil "        ~D: ~S," code name)))))


;;; TODO move the following stuff

;; Given a Lisp form, wrap code mapping signalled Lisp conditions to
;; C-style return values around FORM using the given ERROR-MAP.
(defgeneric wrap-error-handling (form error-map))


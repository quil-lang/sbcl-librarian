(in-package #:sbcl-librarian)

(defun funcall0-by-name (name package-name)
  (funcall (symbol-function (find-symbol (string-upcase name)
                                         (if (string= "" package-name)
                                             (sb-int:sane-package)
                                             (string-upcase package-name)))))
  (values))

(defun set-argv (argv)
  (let ((posix-argv (sb-alien:extern-alien "posix_argv" (* sb-alien:c-string))))
    (setf posix-argv argv)
    (sb-sys:os-cold-init-or-reinit)
    (values)))

(defun load-array-as-system  (data size system-name)
  (unless (asdf:component-loaded-p system-name)
    (uiop:with-temporary-file (:stream stream :pathname filename :direction :io :element-type 'unsigned-byte)
      (loop :for i :from 0 :below size
            :do (write-byte (sb-alien:deref (sb-alien:cast data (* (sb-alien:unsigned 8))) i) stream))
      (finish-output stream)
      (handler-bind
	  ((sb-kernel:redefinition-warning #'muffle-warning))
	(let ((sbcl-librarian::*initialize-callables-p* t))
          (load filename))))
    (asdf:register-immutable-system system-name)
    (values)))

(sbcl-librarian:define-api libsbcl-addons (:function-prefix "")
  (:function
   (("lisp_funcall0_by_name" funcall0-by-name) :void ((name :string) (package-name :string)))
   (("lisp_set_argv" set-argv) :void ((argv :pointer)))
   (("lisp_load_array_as_system" load-array-as-system) :void ((data :pointer) (size :int) (system-name :string)))
   (("lisp_load_shared_object" sb-alien:load-shared-object) :void ((path :string)))
   (("lisp_require_module" require) :void ((module-name :string)))))

(sbcl-librarian:define-aggregate-library libsbcl ()
  libsbcl-addons
  sbcl-librarian:handles
  sbcl-librarian:environment
  sbcl-librarian:loader)

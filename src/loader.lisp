;;;; This file exposes the interface to the Lisp loader, so that we
;;;; can load more Lisp at runtime if desired.

(in-package #:sbcl-librarian)

(defun lisp-load (pathname)
  "Load the file at PATHNAME into the running environment, initializing
any alien callable symbols and muffling any redefinition warnings.

On Darwin, additionally set SB-THREAD::*INITIAL-THREAD* to the current
thread around the LOAD call. This is because CFFI on Darwin forces all
foreign library loads to occur on the initial thread[^1]. If the
current thread when the load happens is the initial thread, this is
trivial. If the current thread is NOT the initial thread, then CFFI
uses SB-THREAD:INTERRUPT-THREAD to invoke the library load on the
initial thread[^2]. INTERRUPT-THREAD is asynchronous, meaning that the
requested function will run on the requested thread sometime in the
future when the requested thread is running and has interrupts
enabled. This does not work when the core file used at startup was
saved with :CALLABLE-EXPORTS because the initial thread never runs
after initializing callable symbols[^3].

[^1]: https://github.com/cffi/cffi/blob/5bfca29deb8b4c214a86ccf37279cc5cea2151e1/src/cffi-sbcl.lisp#L369
[^2]: https://github.com/cffi/cffi/blob/5bfca29deb8b4c214a86ccf37279cc5cea2151e1/src/cffi-sbcl.lisp#L344
[^3]: https://github.com/sbcl/sbcl/blob/6e2df19952cfc3a526dcc42a5c0f8fa6b571f312/src/code/save.lisp#L83"
  (let ((*initialize-callables-p* t)
        (*compile-verbose* nil)
        #+darwin
        (initial-thread sb-thread::*initial-thread*))
    #+darwin
    (setf sb-thread::*initial-thread* sb-thread:*current-thread*)
    (unwind-protect
         (handler-bind
             ((sb-kernel:redefinition-warning #'muffle-warning)
              (style-warning #'muffle-warning))
           (load pathname))
      #+darwin
      (setf sb-thread::*initial-thread* initial-thread))))

(defun load-array-as-system  (data size system-name)
  "Assuming DATA is a pointer to an array of SIZE bytes constituting a
FASL or Common Lisp source code for the entire ASDF system named
SYSTEM-NAME, dump the array to a temporary file and then load it if
SYSTEM-NAME has not already been loaded into the current
image. Additionally, initialize any alien callable symbols while
loading."
  (unless (asdf:component-loaded-p system-name)
    (uiop:with-temporary-file (:stream stream :pathname filename :direction :io :element-type 'unsigned-byte)
      (loop :for i :from 0 :below size
            :do (write-byte (sb-alien:deref (sb-alien:cast data (* (sb-alien:unsigned 8)))
					    i)
			    stream))
      (finish-output stream)
      (lisp-load filename))
    (asdf:register-immutable-system system-name)
    (values)))

(define-api loader (:function-prefix "")
  (:function
   (("lisp_load" lisp-load) :void ((pathname :string)))
   (("lisp_load_array_as_system" load-array-as-system) :void ((data :pointer) (size :int) (system-name :string)))
   (("lisp_require" require) :void ((module-name :string)))
   (("lisp_load_shared_object" load-shared-object) :void ((pathname :string)))))

;;;; This file exposes the interface to the Lisp loader, so that we
;;;; can load more Lisp at runtime if desired.

(in-package #:sbcl-librarian)

(defun lisp-load (pathname)
  (let ((*initialize-callables-p* t)
        (initial-thread sb-thread::*initial-thread*))
    (setf sb-thread::*initial-thread* sb-thread:*current-thread*)
    (unwind-protect
         (load pathname)
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
   (("lisp_require" require) :void ((module-name :string)))))

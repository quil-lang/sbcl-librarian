(require "uiop")

(define-alien-callable funcall0-by-name int ((name c-string))
  (funcall (symbol-function (find-symbol name))))

(define-alien-callable set-argv void ((argc int) (argv (* (c-string #+win32 :external-format #+win32 :ucs-2))))
  (setf sb-sys::*posix-argv*
	(loop :for i :from 0 :below argc
	      :collect (sb-alien:deref argv i))))

(define-alien-callable load-array void ((data (* (unsigned 8))) (size int))
  (uiop:with-temporary-file (:stream stream :pathname filename :element-type '(unsigned-byte 8))
    (loop :for i :from 0 :below size
	  :do (write-byte (deref data i) stream))
    (load filename)))

(let ((runtime-path (first (uiop:command-line-arguments)))
      (shared-lib-suffix (second (uiop:command-line-arguments))))
  (setf (extern-alien "sbcl_runtime" (* t)) (make-alien-string runtime-path))
  (trace sb-alien::initialize-alien-callable-symbol)
  (save-lisp-and-die (concatenate 'string "libsbcl" shared-lib-suffix) :executable t :callable-exports '(funcall0-by-name set-argv load-array)))

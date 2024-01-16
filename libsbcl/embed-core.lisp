(require "asdf")
(require "uiop")

(asdf:load-system :sbcl-librarian)

(define-alien-callable funcall0-by-name void ((name c-string))
  (funcall (symbol-function (find-symbol (string-upcase name)))))

(define-alien-callable set-argv void ((argc int) (argv (* c-string)))
  (setf sb-ext:*posix-argv*
	(loop :for i :from 0 :below argc
	      :collect (sb-alien:deref argv i))))

(define-alien-callable load-array void ((data (* (unsigned 8))) (size int))
  (uiop:with-temporary-file (:stream stream :pathname filename :direction :io :element-type 'unsigned-byte)
    (loop :for i :from 0 :below size
	  :do (write-byte (deref data i) stream))
    (finish-output stream)
    (let ((sbcl-librarian::*initialize-callables-p* t))
      (load filename))))

(let ((runtime-path (first (uiop:command-line-arguments)))
      (shared-lib-suffix (second (uiop:command-line-arguments))))
  (setf (extern-alien "sbcl_runtime" (* t)) (make-alien-string runtime-path))
  (trace sb-alien::initialize-alien-callable-symbol)
  (save-lisp-and-die (concatenate 'string "libsbcl" shared-lib-suffix) :executable t :callable-exports '(funcall0-by-name set-argv load-array)))

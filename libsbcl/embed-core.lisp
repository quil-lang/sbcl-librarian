(require "asdf")
(require "uiop")

(asdf:load-system :sbcl-librarian)
(asdf:register-immutable-system :sbcl-librarian)

(define-alien-callable funcall0-by-name void ((name c-string))
  (funcall (symbol-function (find-symbol (string-upcase name)))))

(define-alien-callable set-argv void ((argc int) (argv (* c-string)))
  (setf sb-ext:*posix-argv*
        (loop :for i :from 0 :below argc
              :collect (sb-alien:deref argv i))))

(define-alien-callable load-array-as-system void ((data (* (unsigned 8))) (size int) (system-name c-string))
  (unless (asdf:component-loaded-p system-name)
    (uiop:with-temporary-file (:stream stream :pathname filename :direction :io :element-type 'unsigned-byte)
      (loop :for i :from 0 :below size
            :do (write-byte (deref data i) stream))
      (finish-output stream)
      (let ((sbcl-librarian::*initialize-callables-p* t))
        (load filename)))
    (asdf:register-immutable-system system-name)))

(define-alien-callable load-shared-object void ((pathname c-string))
  (load-shared-object pathname))

(let ((runtime-path (first (uiop:command-line-arguments)))
      (lib-filename (second (uiop:command-line-arguments))))
  (setf (extern-alien "sbcl_runtime" (* t)) (make-alien-string runtime-path))
  (trace sb-alien::initialize-alien-callable-symbol)
  (save-lisp-and-die lib-filename
                     :executable t
                     :callable-exports '(funcall0-by-name set-argv load-array-as-system load-shared-object)))

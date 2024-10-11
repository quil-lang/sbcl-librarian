(in-package #:sbcl-librarian)

(define-type :json
  :c-type "char*"
  :python-type "c_char_p"
  :alien-type sb-alien:c-string)

(define-enum-type error-type "lisp_err_t"
  ("LISP_ERR_SUCCESS" 0)
  ("LISP_ERR_FAILURE" 1)
  ("LISP_ERR_BUG" 2)
  ("LISP_ERR_FATAL" 3))

(defvar *error-message* ""
  "The most recent error message.")

(defun get-error-message ()
  *error-message*)

(defvar *show-backtrace* nil)

(defun enable-backtrace (code)
  (setf *show-backtrace* (not (zerop code))))

(defun crash ()
  (error "oops"))

(defun exhaust-heap ()
  (sb-sys:without-gcing
    (let ((test '()))
      (loop (push 1 test)))))

(define-error-map default-error-map error-type (:no-error 0 :fatal-error 3)
                  ((cl:warning #'continue)

                   (lisp-error
                    (lambda (c)
                      (when *show-backtrace*
	                (sb-debug:print-backtrace
                         :stream *error-output*
                         :emergency-best-effort t))
                      (setf *error-message* (format nil "~A" c))
                      (return-from default-error-map 1)))

                   (lisp-bug
                    (lambda (c)
                      (when *show-backtrace*
                        (sb-debug:print-backtrace
                         :stream *error-output*
                         :emergency-best-effort t))

                      (let ((*print-backtrace-in-bug* t))
                        (setf *error-message* (format nil "~A" c)))

                      (return-from default-error-map 2)))

                   (cl:error
                    (lambda (c)
                      (let ((bug (make-instance 'lisp-bug
                                                :reason (format nil "~A" c)
                                                :args nil
                                                :backtrace (with-output-to-string (s)
                                                             (sb-debug:print-backtrace
                                                              :stream s
                                                              :emergency-best-effort t)))))

                        (let ((*print-backtrace-in-bug* t))
                          (setf *error-message* (format nil "~A" bug)))

                        (return-from default-error-map 2))))))

(define-api errors ()
  (:literal "/* errors */")
  (:function
   (get-error-message :string ())
   (enable-backtrace :void ((on :int)))
   (crash :void ())
   (exhaust-heap :void ())))

(in-package #:sbcl-librarian)

(defvar *print-backtrace-in-bug* nil)

(define-condition lisp-error (cl:error)
  ((reason :initarg :reason
           :accessor lisp-error-reason)
   (args :initarg :args
         :accessor lisp-error-args))
  (:report (lambda (c s)
             (with-slots (reason args) c
               (let ((*print-circle* nil))
                 (format s "Lisp Error: ~?" reason args))))))

(declaim (inline error))
(defun error (reason &rest args)
  (declare (type string reason))
  (cl:error 'lisp-error
            :reason reason
            :args args))

(define-condition lisp-warning (style-warning)
  ((reason :initarg :reason
           :accessor lisp-warning-reason)
   (args :initarg :args
         :accessor lisp-warning-args))
  (:report (lambda (c s)
             (with-slots (reason args) c
               (let ((*print-circle* nil))
                 (format s "Lisp Warning: ~?" reason args))))))

(declaim (inline warning))
(defun warning (reason &rest args)
  (cl:warn 'lisp-warning
           :reason reason
           :args args))

(define-condition lisp-bug (cl:error)
  ((reason :initarg :reason
           :accessor lisp-bug-reason)
   (args :initarg :args
         :accessor lisp-bug-args)
   (backtrace :initarg :backtrace
              :accessor lisp-bug-backtrace
              :initform (required 'backtrace))
   (context :initform nil
            :accessor lisp-bug-context))
  (:report (lambda (c s)
             (with-slots (reason args backtrace context) c
               (let ((*print-circle* nil))
                 (format s "Internal lisp bug: ~?~%
If you are seeing this, please file an issue and include this error message in the description.
~A

Context:
~{  ~{~A~^: ~}~%~}
~A"
                         reason
                         args
                         "TODO"
                         context
                         (if *print-backtrace-in-bug*
                             backtrace
                             "")))))))

(declaim (inline bug))
(defun bug (reason &rest args)
  (declare (type string reason))
  (cl:error 'lisp-bug
            :reason reason
            :args args
            :backtrace (with-output-to-string (s)
                         (sb-debug:print-backtrace
                          :stream s
                          :start 1      ; Don't show the `bug' call
                          :emergency-best-effort t))))

(declaim (inline unreachable))
(defun unreachable ()
  (bug "unreachable"))

(defmacro with-bug-context (context &body body)
  `(handler-bind
       ((lisp-bug
          (lambda (c)
            (declare (ignorable c))
            ,@(mapcar (lambda (ctx)
                        (cl:assert (= 2 (length ctx)))
                        `(push (list ,@ctx) (lisp-bug-context c)))
                      context))))
     ,@body))

(defmacro assert (test-form &optional places datum arguments)
  (let ((sym (gensym)))
    `(with-bug-context (,@(mapcar (lambda (place)
                                    `(,(format nil "~S" place) ,place))
                                  places))
       (let ((,sym ,test-form))
         (unless ,sym
           (bug ,(or datum "The assertion ~S failed")
                ,@(if datum arguments `(',test-form))))))))

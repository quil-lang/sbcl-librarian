(in-package #:sbcl-librarian)

(defun memory-report ()
  (format t "~&output of (ROOM NIL):~%")
  (room nil)
  (format t "~&~%output of (DESCRIBE-HANDLES):~%")
  (describe-handles)
  (values))

(defun describe-handles ()
  (sb-thread:with-mutex (*handle-lock*)
    (format t "SBCL-LIBRARIAN HANDLES~%")
    (format t "     HANDLE | TYPE~%")
    (loop :for handle :being :the :hash-keys :of (aref *handles* 0)
            :using (:hash-value object)
          :do (format t " ~10d | ~S~%" handle (type-of object)))
    (format t "~%"))
  (values))

(defun handle-count ()
  (sb-thread:with-mutex (*handle-lock*)
    (hash-table-count (aref *handles* 0))))

(defun start-swank-server (port)
  (sb-ext:enable-debugger)
  (uiop:symbol-call "SWANK" "CREATE-SERVER" :port port :dont-close t)
  (values))

(defun perform-gc ()
  (sb-ext:gc :full t)
  (values))

(defun start-profiling (args)
  #-win32
  (apply #'sb-sprof:start-profiling (read-from-string args))
  #+win32
  (format t "SB-SPROF is not supported on Windows."))

(defun profiler-report (args)
  #-win32
  (apply #'sb-sprof:report (read-from-string args))
  #+win32
  (format t "SB-SPROF is not supported on Windows."))

(define-api diagnostics (:function-prefix "")
  (:function
   (("lisp_memory_report" memory-report) :void ())
   (("lisp_dynamic_usage" sb-kernel:dynamic-usage) :uint64 ())
   (("lisp_describe_handles" describe-handles) :void ())
   (("lisp_handle_count" handle-count) :int ())
   (("lisp_start_swank_server" start-swank-server) :void ((port :int)))
   (("lisp_start_profiling" start-profiling) :void ((args :string)))
   (("lisp_stop_profiling" sb-sprof:stop-profiling) :void ())
   (("lisp_profiler_report" profiler-report) :void ((args :string)))
   (("lisp_reset_profiler" sb-sprof:reset) :void ())))

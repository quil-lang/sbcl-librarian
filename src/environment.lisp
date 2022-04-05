;;;; This file exposes some environment related functions to the Lisp
;;;; runtime, so that we can manage things like Lisp threads, signals,
;;;; and entry into the Lisp debugger for interaction with foreign
;;;; code.

(in-package #:sbcl-librarian)

(defun enable-debugger ()
  (setq sb-ext:*invoke-debugger-hook*
        (lambda (condition old-hook)
          (let ((sb-ext:*invoke-debugger-hook* nil))
            ;; This is a huge hack messing with the internals of the
            ;; session thread state in SBCL. SBCL doesn't know that
            ;; foreign callback threads could ever want to hijack the
            ;; main thread and become interactive themselves for
            ;; debugging. The session data-structure is also usually
            ;; protected by a lock, which we're not doing here because
            ;; this is a hack.
            (push sb-thread::*current-thread*
                  (sb-thread::session-interactive-threads sb-thread::*session*))
            (invoke-debugger condition))))
  (sb-ext:enable-debugger))

(defun disable-debugger ()
  (sb-ext:disable-debugger))

(defun gc ()
  (gc :full t))

(define-api environment (:function-prefix "")
  (:function
   (("lisp_enable_debugger" enable-debugger) :void ())
   (("lisp_disable_debugger" disable-debugger) :void ())
   (("lisp_gc" gc) :void ())))

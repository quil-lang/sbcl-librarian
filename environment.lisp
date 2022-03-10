;;;; This file exposes some environment related functions to the Lisp
;;;; runtime, so that we can manage things like Lisp threads, signals,
;;;; and entry into the Lisp debugger for interaction with foreign
;;;; code.

(in-package #:sbcl-librarian)

(defun enable-debugger ()
  (setq sb-ext:*invoke-debugger-hook*
        (lambda (condition old-hook)
          (let ((sb-ext:*invoke-debugger-hook* nil)
                (session sb-thread::*session*)
                (current-thread sb-thread::*current-thread*))
            ;; This is a huge hack messing with the internals of the
            ;; session thread state in SBCL. SBCL doesn't know that
            ;; foreign callback threads could ever want to hijack the
            ;; main thread and become interactive themselves for
            ;; debugging.
            (symbol-macrolet ((interactive-threads
                                (sb-thread::session-interactive-threads session)))
              (let ((this-thread current-thread))
                (unless (member current-thread interactive-threads)
                  (setf interactive-threads
                        (list current-thread)))))
            (invoke-debugger condition))))
  (sb-ext:enable-debugger))

(define-api environment (:function-prefix "")
  (:function
   (("lisp_enable_debugger" enable-debugger) :void ())))

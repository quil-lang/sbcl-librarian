(in-package #:sbcl-librarian)

;;; Used to coordinate Lisp objects in C gracefully GC-wise. We may be
;;; able to do some optimizations here like noticing that immediate
;;; objects like FIXNUMs are GC-safe and do not need to be coerced
;;; into handles.
(defvar *handles* (vector (make-hash-table) 0))

;; coerce int key to void* handle
(defun key-handle (key) (sb-alien:sap-alien (sb-int::int-sap key) (* t)))
(defun handle-key (handle) (sb-alien::sap-int (sb-alien:alien-sap handle)))

(defun make-handle (object)
  (let* ((handles *handles*)
         (key (svref handles 1)))
    (setf (gethash key (svref handles 0)) object)
    (setf (svref handles 1) (1+ key))
    (key-handle key)))

(defun dereference-handle (handle)
  (gethash (handle-key handle) (svref *handles* 0)))

(defun release-handle (handle)
  (remhash (handle-key handle) (svref *handles* 0))
  (values))

(defun handle-eq (handle-a handle-b)
  (eq (dereference-handle handle-a) (dereference-handle handle-b)))

(define-api handles (:function-prefix "")
  (:function
   (("lisp_release_handle" release-handle) :void ((handle :pointer)))
   (("lisp_handle_eq" handle-eq) :bool ((a :pointer) (b :pointer)))))

;;;; libminiqvm.lisp

(in-package #:libminiqvm)

(sbcl-librarian:define-handle-type qvm-type "qvm")
(sbcl-librarian:define-handle-type quil-program-type "qvm_quil_program")

(sbcl-librarian:define-enum-type error-type "miniqvm_err_t"
  ("MINIQVM_ERR_SUCCESS" 0)
  ("MINIQVM_ERR_FAIL" 1))
(sbcl-librarian:define-error-map error-map error-type 0
  ((t (lambda (condition)
        (declare (ignore condition))
        (return-from error-map 1)))))

(defun amplitudes (qvm)
  (format nil "~A" (qvm::amplitudes qvm)))

(sbcl-librarian:define-library libminiqvm (:error-map error-map
                                           :function-linkage "MINIQVM_"
                                           :function-prefix "miniqvm_")
  (:type qvm-type quil-program-type error-type)
  (:function
   (("run_program" qvm:run-program) qvm-type ((num-qubits :int) (program quil-program-type)))
   (("amplitudes" amplitudes) :string ((qvm qvm-type)))))

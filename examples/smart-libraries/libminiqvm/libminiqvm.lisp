;;;; libminiqvm.lisp

(in-package #:libminiqvm)

(sbcl-librarian:define-handle-type qvm-type "qvm")
(sbcl-librarian:define-handle-type quil-program-type "quil_program")

(sbcl-librarian:define-library libminiqvm (:function-linkage "MINIQVM_"
                                           :function-prefix "miniqvm_")
  (:type qvm-type quil-program-type)
  (:function
   (("run_program" qvm:run-program) qvm-type ((num-qubits :int) (program quil-program-type)))
   (("amplitudes" qvm::amplitudes) :string ((qvm qvm-type)))))

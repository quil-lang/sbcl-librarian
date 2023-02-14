;;;; consplosion.lisp -- a function to cons much from the external world.

(in-package :sbcl-librarian.tests.consplosion)

(defclass consbomb ()
  ((payload :initarg :payload
            :accessor payload)))

(defun consplode (n)
  "Just cons a bunch of random numbers and return the list"
  (labels ((rec (n &optional acc) (if (plusp n) (rec (1- n) (cons (random pi) acc)) acc)))
    (make-instance 'consbomb :payload (rec n))))


(defun consplode-hashtable (n)
  "Just make a hash table filled with random numbers and return it as a consbomb instance"
  (let ((tab (make-hash-table :test 'equal)))
    (dotimes (i n (make-instance 'consbomb :payload tab))
      (setf (gethash i tab) (random #xFFFF)))))

(defun consplode-vector (n)
  "Just make a vector of floats size n, return a consbomb with that vector as a the payload"
  (make-instance 'consbomb :payload (make-array n :element-type 'double-float)))

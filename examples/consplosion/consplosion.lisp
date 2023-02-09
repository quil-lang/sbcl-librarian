;;;; consplosion.lisp -- a function to cons much from the external world.

(in-package :sbcl-librarian.tests.consplosion)

(defclass consbomb ()
  ((consed :initarg :consed
           :accessor consed)))

(defun consplode (n)
  "Just cons a bunch of random numbers and return the list"
  (labels ((rec (n &optional acc) (if (plusp n) (rec (1- n) (cons (random pi) acc)) acc)))
    (make-instance 'consbomb :consed (rec n))))



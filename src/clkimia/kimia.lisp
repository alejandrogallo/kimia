(defpackage :kimia
  (:use :cl))

(in-package :kimia)

(defun test ()
  (print "Hello From Kimia"))

(defmacro wrap-input-script (arg)
  `(progn
     (print "LISP:: START evaluating KIMIA script")
     ,arg
     (print "LISP:: DONE evaluating KIMIA")))

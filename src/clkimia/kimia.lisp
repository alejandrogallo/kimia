(defpackage :kimia
  (:use :cl)
  (:import-from :ka)
  (:nicknames :k))

(in-package :kimia)

(defparameter *KIMIA_STEPS* '())

(ka::algorithm)

(defmacro @step (s)
  `(setf *KIMIA_STEPS* (cons ,s *KIMIA_STEPS*)))


(defmacro wrap-input-script (&rest arg)
  `(progn
     (import 'kimia)
     (princ "LISP:: START evaluating KIMIA script")
     ,@arg
     (princ "LISP:: DONE evaluating KIMIA")
     *KIMIA_STEPS*))

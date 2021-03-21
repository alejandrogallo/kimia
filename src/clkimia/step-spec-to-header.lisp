(load "kimia.lisp")
(load "flagis.lisp")
(in-package :kimia)

(when (cl-user::flag-is-given "-h" (cl-user::argv))
  (format t "Usage: -i input-header-file -o output-header-file~%")
  (ext:quit 0))

(in-package :kimia)

(let ((in-file (cl-user::flag-value "-i" (cl-user::argv)))
      (out-file (cl-user::flag-value "-o" (cl-user::argv))))
  (assert out-file nil "~%~tOutfile missing (-o)~%")
  (assert in-file nil "~%~tIn file missing (-i)~%")

  (load in-file)

  (dolist (spec kimia::*kimia-step-specs*)

    (with-open-file (f out-file :direction :output
                                :if-exists :supersede)
      (format f "~&#pragma once~%~%")
      (format f "~&~a" (step/translate-struct :c++ (getf (eval spec) :name))))))

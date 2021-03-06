(in-package :cl-user)
(defun flag-value (flag args &optional (default nil) &key (multiple nil))
  (cond ((string-equal flag (car args))
         (if multiple
             (cons (cadr args)
                   (flag-value flag (cdr args) default :multiple multiple))
             (cadr args)))
        ((null args) default)
        (t (flag-value flag (cdr args) default :multiple multiple))))

(defun flag-is-given (flag args)
  (member flag args :test #'string-equal))

(defun argv () (ext:command-args))

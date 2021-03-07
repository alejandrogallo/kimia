(defpackage :kimia/algorithm
  (:nicknames :ka)
  (:export :mk-algorithm
           :algorithm))

(in-package :kimia/algorithm)

(defmacro algorithm ()
  `(print "Algorithm"))

(defun consume-in-out (lst &optional (tail '()))
  (let ((first (car lst))
        (rest (cdr lst)))
    (cond
      ((eq first :out) `(,(reverse tail) ,rest))
      ((eq first :in) (consume-in-out rest tail))
      ((eq lst '()) `(,(reverse tail) ,rest))
      (t (consume-in-out rest (cons first tail))))))

(defmacro mk-algorithm (name &rest args)
  (let* ((in-out (consume-in-out args))
         (in (car in-out))
         (out (cadr in-out)))
    `'(',name
       :in ',in
       :out ',out)))

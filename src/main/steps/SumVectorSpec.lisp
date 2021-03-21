(in-package :kimia)

(defstep (sum-vector F)
  :in
  (:name :vector
   :doc "Vector to be summed"
   :type (vec F))
  :out
  (:name :sum
   :doc "The sum of the vector"
   :type F)
  :run
  (run-sum-vector F))

(step/instantiate '(sum-vector double-float))
(step/instantiate '(sum-vector single-float))
(step/instantiate '(sum-vector integer))

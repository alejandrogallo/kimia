(in-package :kimia)

(in-package :kimia.types)
(let (step)
  (setq step
        '(:name :mode
          :type (member :binary :text)
          :default :binary
          :required t
          :doc "The file where the tensor is located"))
  (check-type step step-setting-spec))
(defstep
  tensor-reader
  :in
  (:name :file
   :type (or string null)
   :default nil
   :required t
   :doc "The file where the tensor is located")
  (:name :mode
   :type (member :binary :text)
   :default :binary
   :required t
   :doc "The file where the tensor is located")
  :out
  (:name :tensor
   :type string
   :default "out.tensor"
   :required t
   :doc "The file where the tensor is located"))

(check-type (tensor-reader-default)
            tensor-reader)
(let (step default)
  (setq step
        '(:name Tensor-Reader
          :in (:file "asdf"
               :mode :binary)
          :out (:tensor "Integral")))
  (setq default
        (tensor-reader-default))

  (check-type default tensor-reader)
  (check-type step tensor-reader))

(let ((step (tensor-reader-default)))
  (eval `(check-step-type ,step)))

(mk-step
 'Tensor-Reader
 :in
 :file "amplitudes.dat"
 :mode :binary
 :out
 :tensor "Whatever")

(mk-stepq
 Tensor-Reader
 :in
 :file "amplitudes.dat"
 :mode :binary
 :out
 :tensor "Whatever")

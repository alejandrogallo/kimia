(in-package :kimia)

(defstep tensor-reader
  :in
  (:name :file
   :type string
   :default "input.dat"
   :required t
   :doc "The file where the tensor is located")
  (:name :mode
   :type (member :binary :text)
   :default :binary
   :required t
   :doc "The encoding and format that the tensor is written in")
  :out
  (:name :data
   :type integer
   :default 42
   :required t
   :doc "Just a test setting"))

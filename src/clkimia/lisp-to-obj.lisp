(require 'cmp)
(load "flagis.lisp")

(let ((in-file (flag-value "-i" (argv))))
  (assert in-file)
  (format t
          "~2%~a :: ~w ~2%"
          "Compile lisp file into object file"
          in-file)
  (compile-file in-file :system-p t))

(ext:quit 0)

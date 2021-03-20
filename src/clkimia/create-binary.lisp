(require 'cmp)
(load "flagis.lisp")

(format t "~4%~a~4%" "Compile object files into static library")

(let ((in-files (flag-value "-i" (argv) nil :multiple t))
      (out-file (flag-value "-o" (argv))))
  (assert out-file) (assert in-files)

  (c:build-program out-file
                   :lisp-files in-files))

(ext:quit 0)

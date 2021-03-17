(require 'cmp)
(load "flagis.lisp")

(format t "~4%~a~4%" "Compile object files into static library")

(let ((in-files (flag-value "-i" (argv) nil :multiple t))
      (out-file (flag-value "-o" (argv)))
      (init-name (flag-value "--init" (argv))))
  (assert init-name) (assert out-file) (assert in-files)

  (c:build-static-library out-file
                          :lisp-files in-files
                          :init-name init-name))

(ext:quit 0)

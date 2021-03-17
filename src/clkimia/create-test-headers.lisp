;; taritz: ./ecl --shell %
(load "kimia.lisp")
(load "test-data.lisp")

(in-package :kimia)

(defparameter *structs-to-export*
      '(tensor-reader-double
        (monster-struct integer double-float integer)
        ))

(defparameter *translated-types* nil)

(defun maybe-print-definitions (ty)
  (unless (or (struct-unnamed-p ty)
              (when (consp ty) (struct-unnamed-p (cadr ty)))
              (member ty *translated-types* :test #'equal))
    (push ty *translated-types*)
    (format t "~&/* FOR: ~a (~a) */" ty (translate :c++ ty))
    (format t "~&~1%~a" (define :c++ ty))
    (format t "~&~a~2%" (caster-snippet :c++ ty))))

(defun translate-type-dependencies (type)
  (let ((subs (subtypes :c++ type)))
    ;(format t "~&/* ~a ↔ ~a*/" type subs)
    (when subs
      (if (consp subs)
        (dolist (stype subs) (translate-type-dependencies stype))
        (translate-type-dependencies subs)))
    (maybe-print-definitions type)

    ))

;(trace translate-type-dependencies)

(dolist (name *structs-to-export*)
  (let* ((identifier `(struct ,name))
         (ty-name (struct-spec-name identifier))
         (spec (struct-get-expanded-spec identifier))
         (generic-spec (struct-get-spec `(struct ,ty-name))))
    (translate-type-dependencies spec)))

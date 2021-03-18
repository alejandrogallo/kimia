;; taritz: ./ecl --shell %
(load "kimia.lisp")
(load "test-data.lisp")

(in-package :kimia)

(defparameter *structs-to-export*
      '(tensor-reader-double
        (tensor-reader integer)
        (tensor-reader double-float)
        (tensor-reader single-float)
        (tensor-reader (vec integer))
        (Uttu string)
        (Uttu (vec string))
        ;(Uttu (struct (Uttu string)))
        (davidson-solver integer double-float double-float integer)
        (monster-struct integer double-float integer)
        ))

(defparameter *translated-types* nil)

(defun maybe-print-definitions (ty)
  (unless (or (struct-unnamed-p ty)
              (when (consp ty) (struct-unnamed-p (cadr ty)))
              (member ty *translated-types* :test #'equal))
    (push ty *translated-types*)
    ;(format t "~&/* FOR: ~a (~a) */~&" ty (translate :c++ ty))
    (format t "~&~1%~a" (define :c++ ty))
    (ignore-errors
     ;; for templated types this will raise errors since in general
     ;; there is no general implementation for all types
     (format t "~&~a~2%" (caster-snippet :c++ ty)))))

(defun translate-type-dependencies (type)
  (let ((subs (subtypes :c++ type)))
    ;(format t "~&/* ~a ↔ ~a*/" type subs)
    (when subs
      (if (consp subs)
        (dolist (stype subs) (translate-type-dependencies stype))
        (translate-type-dependencies subs)))
    (maybe-print-definitions type)))

(defun print-translate-generic-struct (struct-identifier)
  (let* ((spec (struct-get-spec struct-identifier))
         (is-unnamed (struct-unnamed-p struct-identifier))
         (is-generic (struct-spec-generic-p spec))
         (gvars (struct-spec-generic-vars spec))
         (safe-gvars (mapcar (lambda (g)
                               (if (generic-p g)
                                   g
                                   `(g ,(c++-type-name g))))
                             gvars))
         (subst-list (pairlis safe-gvars gvars)))
    ;(format t "~&//gvars: ~a" gvars)
    (if (and gvars
             (not is-unnamed))
        (maybe-print-definitions (rec-subst subst-list spec)))
    ))

;(trace translate-type-dependencies)

(dolist (name *structs-to-export*)
  (let* ((identifier `(struct ,name))
         (ty-name (struct-spec-name identifier))
         (spec (struct-get-expanded-spec identifier))
         (generic-spec (struct-get-spec `(struct ,ty-name))))
    (print-translate-generic-struct identifier)
    (translate-type-dependencies spec)))

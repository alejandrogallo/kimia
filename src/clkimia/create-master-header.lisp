;; taritz: ./ecl --shell %
(load "kimia.lisp")
(load "flagis.lisp")

(setf in-files (flag-value "-i" (argv) nil :multiple t))
(setf out-file (flag-value "-o" (argv)))
(assert out-file) (assert in-files)


(dolist (infile in-files)
  (load infile))

(in-package :kimia)

(defparameter *immediate-headers*
  (mapcar (lambda (x) (replace-all x ".lisp" ".hpp")) cl-user::in-files))

(defparameter *c++-headers*
  '(string map vector array cstdlib ecl/ecl.h))

(defparameter *structs-to-export*
  *KIMIA-STEP-INSTANTIATIONS*)

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





(with-open-file (*standard-output* cl-user::out-file :direction :output
                                                     :if-exists :supersede)
  (format t "~{~&#include<~(~a~)>~}" *c++-headers*)
  (format t "~{~&#include \"~a\"~}" *immediate-headers*)
  (format t "~&~&std::map<std::string, size_t> DATABASE;")
  (format t "~&~&std::map<std::string, size_t> RUNNER_TO_CASTER;")
  (dolist (name *structs-to-export*)
    (let* ((identifier `(struct ,name))
           (ty-name (struct-spec-name identifier))
           (spec (struct-get-expanded-spec identifier))
           (generic-spec (struct-get-spec `(struct ,ty-name))))
      ;(print-translate-generic-struct identifier)
      (translate-type-dependencies spec)))

  ;; setup function database
  (format t "~&void setupRunnerDatabase(void) {")
  (dolist (name *structs-to-export*)
    (format t "~&  /* ~s */" name)
    (format t "~&  DATABASE[\"~a\"] = (size_t)&~:*~a;"
            (step/run-function-name :c++ name))
    (format t "~&  RUNNER_TO_CASTER[\"~a\"] = (size_t)~a;"
            (step/run-function-name :c++ name)
            (step/caster-name :c++ name))
    )
  (format t "~&}")

  )

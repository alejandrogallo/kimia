(defpackage :kimia
  (:use :cl)
  (:nicknames :k))
(in-package :kimia)

(defun endl () (format nil "~%"))

(defun c++-type-name (thing)
  (remove-if (lambda (x) (string= x "-"))
             (string-capitalize
              (string-downcase thing))))

(defun c-type-name (thing)
  (concatenate
   'string
   (substitute #\_ #\-
               (string-downcase thing))
   "_t"))

(defun c++-var-name (thing)
  (nstring-downcase
   (remove-if (lambda (x) (string= x "-"))
              (string-capitalize
               (string-downcase thing)))
   :start 0
   :end 1))

(defun c-var-name (thing)
  (concatenate
   'string
   (substitute #\_ #\-
               (string-downcase thing))))
(defparameter *KIMIA-TYPES* '())

(defmacro declare-var-fn-default (lang translate)
  (ecase lang
    ((:c :c++)
     `(lambda (ty vn)
        (format nil "~a ~a;"
                (funcall ,translate ty)
                (c++-var-name vn))))))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun caster-signature-fmt (lang)
    (ecase lang
      ((:c :c++)
       "~&size_t ~a (const cl_object o)")))

  (defun caster-envelope-fmt (lang)
    (ecase lang
      ((:c :c++)
       (concatenate 'string
                    "~&~a"
                    (caster-signature-fmt lang)
                    "{~&~a~&}"))))

  (defun internal-type-name (type)
    (string-downcase
     (etypecase type
       (cons (format nil "~A-~A"
                     (car type)
                     (length type)))
       (symbol type))))

  (defun defequiv-var-name (lang type)
    (check-type lang keyword)
    (intern (format nil "~@:(~A-~A~)"
                    (internal-type-name type)
                    lang)))

  (defun defequiv-spec (lang type)
    (let ((var (defequiv-var-name lang type)))
      (if (boundp var)
          (eval var)
          (error (format nil "No equivalence found for type ~a for lang ~a"
                         type lang)))))
  )

(defmacro defequiv-alias (lang type from-type)
  (let ((new-spec-name (defequiv-var-name lang type))
        (spec-name (defequiv-var-name lang from-type)))
    `(setq ,new-spec-name ,spec-name)))

(defmacro defequiv-from (lang type &rest args &key from &allow-other-keys)
  (remf args :from)
  (flet ((fun-or-scalar (thing)
           (etypecase thing
             (cons (eval thing))
             (compiled-function thing)
             ((or null string) (eval `(lambda (&optional args)
                                        ,thing))))))
    (let* ((new-spec-name (defequiv-var-name lang type))
           (spec (copy-seq (defequiv-spec lang from)))
           (keys (get-keys args)))
      (dolist (key keys)
        (unless (null (getf args key))
          (setf (getf spec key) (fun-or-scalar (getf args key)))))
      `(setq ,new-spec-name ',spec))))

(defmacro defequiv (lang type
                    &key
                      translate
                      (declare-var nil)
                      (define "")
                      (subtypes nil)
                      (caster-header "")
                      (caster-body "")
                      caster-name
                      caster-snippet)
  (flet ((fun-or-scalar (thing)
           (etypecase thing
             (cons (eval thing))
             (compiled-function thing)
             ((or null string) (eval `(lambda (&optional args)
                                        ,thing))))))
    (let* ((type-lang-name (defequiv-var-name lang type))
           (translate (fun-or-scalar translate))
           (caster-header-f (fun-or-scalar caster-header))
           (caster-name-f (fun-or-scalar caster-name))
           (subtypes-f (fun-or-scalar subtypes))
           (caster-body-f (fun-or-scalar caster-body))
           (caster-snippet-f
             (if caster-snippet
                 (fun-or-scalar caster-snippet)
                 (lambda (ty) (format
                               nil
                               (caster-envelope-fmt lang)
                               (funcall caster-header-f ty)
                               (funcall caster-name-f ty)
                               (indent 2 (funcall caster-body-f ty))))))
           (declare-var (or declare-var (eval
                                         `(declare-var-fn-default ,lang
                                                                  ,translate)))))
      `(progn
         (defparameter ,type-lang-name nil)
         (setq ,type-lang-name
               '(:translate ,translate
                 :declare-var ,(fun-or-scalar declare-var)
                 :define ,(fun-or-scalar define)
                 :subtypes ,subtypes-f
                 :caster-header ,caster-header-f
                 :caster-name ,caster-name-f
                 :caster-body ,caster-body-f
                 :caster-snippet ,caster-snippet-f)))
      )))

;; TODO: generalize these funcs
(defun caster-snippet (lang ty)
  (let ((spec (defequiv-spec lang ty)))
    (funcall (getf spec :caster-snippet) ty)))

(defun subtypes (lang ty)
  (let* ((spec (defequiv-spec lang ty))
         (subtypes (funcall (getf spec :subtypes) ty)))
    subtypes))

(defun caster-body (lang ty)
  (let ((spec (defequiv-spec lang ty)))
    (funcall (getf spec :caster-body) ty)))

(defun caster-name (lang ty)
  (let ((spec (defequiv-spec lang ty)))
    (funcall (getf spec :caster-name) ty)))

(defun translate (lang ty)
  (let ((spec (defequiv-spec lang ty)))
    (funcall (getf spec :translate) ty)))

(defun define (lang ty)
  (let ((spec (defequiv-spec lang ty)))
    (funcall (getf spec :define) ty)))

(defun declare-var (lang ty vn)
  (let ((spec (defequiv-spec lang ty)))
    (funcall (getf spec :declare-var) ty vn)))

(defun caster-signature (lang ty)
  (let ((fmt (format nil "~a;" (caster-signature-fmt lang))))
    (format nil
            fmt
            (caster-name lang ty))))
(defequiv :c++ integer
  :translate "int"
  :caster-name (lambda (ty) (format nil "cl~a" (translate :c++ ty)))
  :caster-body (lambda (ty)
                    (format nil "return (size_t)new int(ecl_to_int(o));"
                            (translate :c++ ty))))

(defequiv :c++ double-float
  :translate "double"
  :caster-name (lambda (ty) (format nil "cl~a" (translate :c++ ty)))
  :caster-body "return (size_t)new double(ecl_to_double(o));")

(defequiv :c++ single-float
  :translate "float"
  :caster-name (lambda (ty) (format nil "cl~a" (translate :c++ ty)))
  :caster-body "return (size_t)new float(ecl_to_float(o));")

(defequiv :c++ boolean
  :translate "bool"
  :caster-name (lambda (ty) (format nil "cl~a" (translate :c++ ty)))
  :caster-body "return (size_t)new bool(ecl_to_bool(o));")

;; TODO: caster body
(defequiv :c++ string
  :translate "std::string"
  :caster-body "const size_t dimension(o->base_string.dim);
                std::string result;
                ecl_base_char* c = o->base_string.self;
                // TODO: handle the unicode well.
                // right now I know it is 32bit characters,
                // that is why the i * 4 is there
                for (size_t i = 0; i < dimension; i++)
                  result += *(c + i * 4);
                return (size_t)new std::string(result);"

  :caster-name "clstr")
(defparameter +c++-vector-body+
"~a result(ecl_to_int(cl_length(o)));
"for (size_t i=0; i < result.size(); i++) {
"  cl_object index(c_string_to_object(std::to_string(i).c_str()));
"  ~a *element = (~a*)~a(cl_aref(2, o, index));
"  result[i] = *element;
"}
"return (size_t)new ~a(result);")

(defun vec-p (F v)
  (every (lambda (x) (typep x F))
         v))

(deftype vec (F &optional N)
  `(and (array * (,N)) ; take care of the types in vec
    (satisfies ,(lambda (x) (vec-p F x)) )))

(defequiv :c++ (vec F)
  :translate (lambda (ty)
               (format nil "std::vector< ~a >"
                       (translate :c++ (cadr ty))))

  :subtypes (lambda (ty) `(,(cadr ty)))

  :caster-name (lambda (ty)
                 (format nil "v_of_~a"
                         (caster-name :c++ (cadr ty))))

  :caster-header (lambda (ty) (caster-signature :c++ (cadr ty)))

  :caster-body
     (lambda (ty)
       (format nil
               +c++-vector-body+
               (translate :c++ ty)
               (translate :c++ (cadr ty))
               (translate :c++ (cadr ty))
               (caster-name :c++ (cadr ty))
               (translate :c++ ty)
               )))

(defequiv :c++ (vec F N)

  :translate (lambda (ty)
               (format nil "std::array< ~a, ~a >"
                       (translate :c++ (cadr ty))
                       (caddr ty)))

  :subtypes (lambda (ty) `(,(cadr ty)))

  :caster-name (lambda (ty)
                 (format nil "ar_of_~a_~a"
                         (caddr ty)
                         (caster-name :c++ (cadr ty))))
  :caster-body
     (lambda (ty)
       (format nil
               +c++-vector-body+
               (translate :c++ ty)
               (translate :c++ (cadr ty))
               (translate :c++ (cadr ty))
               (caster-name :c++ (cadr ty))
               (translate :c++ ty)
               )))


;; generic variables
(defequiv :c++ (G N)
  :translate (lambda (ty)
               (format nil "_G~a" (cadr ty))))

(defun generic-p (type)
  (etypecase type
    (cons (eq 'g (car type)))
    (t nil)))
(defun pointer-p (ty ps)
  (and (eq (car ps) 'pointer)
       (let ((value (cadr ps)))
         (etypecase value
           (symbol (if (boundp value)
                       (typep (eval value) ty)
                       t))
           (t (typep value ty))))))

(deftype pointer (type-pointed-to)
  `(and cons
        (satisfies ,(lambda (x) (pointer-p type-pointed-to x)))))

;; TODO: create the real caster body
(defequiv :c++ (pointer F)
  :translate (lambda (ty) (format nil "~a*" (translate :c++ (cadr ty))))
  :caster-body (lambda (ty) (format nil "return (size_t)new size_t(~a(o));"
                                    (caster-name :c++ (cadr ty))))
  :subtypes (lambda (ty) `(,(cadr ty)))
  :caster-header (lambda (ty) (caster-signature :c++ (cadr ty)))
  :caster-name (lambda (ty) (format nil "p~a" (caster-name :c++ (cadr ty)))))

(deftype const (type-pointed-to)
  `(satisfies ,(lambda (x) (typep x type-pointed-to))))

(defequiv :c++ (const F)
  :translate (lambda (ty) (format nil "const ~a" (translate :c++ (cadr ty))))
  :caster-name (lambda (ty) (format nil "c~a" (caster-name :c++ (cadr ty))))
  :subtypes (lambda (ty) `(,(cadr ty)))
  :caster-header (lambda (ty) (caster-signature :c++ (cadr ty)))
  :caster-body (lambda (ty) (caster-body :c++ (cadr ty))))
(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun struct-spec-p (ty)
    (and (eq (car ty) 'struct)
         (typep (cadr ty) '(or cons symbol))
         (typep (caddr ty) '(or cons symbol))
         (eql (length ty) 3)))

  (defun struct-identifier-p (ty)
    (and (eq (car ty) 'struct)
         (typep (cadr ty) '(or cons symbol))
         (eql (length ty) 2)))

  (deftype struct-identifier ()
    '(and cons
      (satisfies struct-identifier-p)))

  (deftype struct-spec ()
    '(and cons
      (satisfies struct-spec-p)))

  (defun struct-spec-name (ty)
    (check-type ty (or struct-spec struct-identifier))
    (let ((name (cadr ty)))
      (typecase name
        (symbol name)
        (cons (car name)))))

  (defun struct-spec-generic-vars (ty)
    (check-type ty (or struct-spec struct-identifier))
    (etypecase (cadr ty)
      (cons (cdadr ty))
      (t nil)))

  (defun struct-spec-fields (ty)
    (check-type ty struct-spec)
    (caddr ty))

  (defun struct-template-line (ty)
    (check-type ty (or struct-spec struct-identifier))
    (let ((gvars (struct-spec-generic-vars ty)))
      (if gvars
          (if (remove-if-not #'generic-p gvars)
              (format nil "template < ~{typename ~a~^, ~} >"
                      (mapcar (lambda (x) (translate :c++ x)) gvars))
              "template")
          "")))

  (defun struct-spec-symbol (struct-name)
    (check-type struct-name (or symbol string))
    (intern
     (format nil "~@:(~A-SPEC~)"
             struct-name)))

  (defun struct-spec-subtypes (spec)
    (etypecase spec
      (struct-spec (mapcar #'cadr (struct-spec-fields spec)))
      (struct-identifier (let* ((name (struct-spec-name spec))
                                (spec-symbol (struct-spec-symbol name))
                                (spec (eval spec-symbol)))
                           (struct-spec-subtypes spec)))))

  (defun rec-subst (ls what)
    (check-type ls list)
    (check-type what cons)
    (let ((pair (car ls)))
      (etypecase pair
        (null what) ;; We are done
        (cons (rec-subst (cdr ls)
                         (subst (car pair) (cdr pair) what))))))

  (defun struct-unnamed-p (ty)
    (and (typep ty '(or struct-spec struct-identifier))
         (null (struct-spec-name ty))))

  (defun struct-get-spec (ty)
    (check-type ty (or struct-spec struct-identifier))
    (if (struct-unnamed-p ty)
        ty
        (eval (struct-spec-symbol (struct-spec-name ty)))))

  (defun struct-get-expanded-spec (ty)
    (let* ((spec (struct-get-spec ty))
           (gvars (struct-spec-generic-vars ty))
           (spec-gvars (struct-spec-generic-vars spec))
           (equivalence-list (pairlis gvars spec-gvars)))
      (if equivalence-list
          (rec-subst equivalence-list spec)
          spec)))

  (defun struct-spec-generic-p (spec)
    (check-type spec (or struct-spec struct-identifier))
    (let ((gvars (struct-spec-generic-vars spec)))
      (remove-if-not #'generic-p gvars)))

  (defun translate-struct-c++ (ty)
    (let* ((ty-name (struct-spec-name ty))
           (name (if ty-name (c++-type-name ty-name) ""))
           (specialized-spec (struct-get-expanded-spec ty))
           (fields (struct-spec-fields specialized-spec))
           (gvars (struct-spec-generic-vars specialized-spec))
           (is-generic (struct-spec-generic-p specialized-spec)))
      (format nil "~&~a~&struct ~a~a~a"
              (struct-template-line ty)
              (or name "")
              (cond
                ((and gvars
                      (not is-generic))
                 (format nil "< ~{~a~^, ~} >"
                         (mapcar (lambda (ty) (translate :c++ ty))
                                 gvars)))
                (t
                 (format nil " {~{~&~a~}}"
                         (loop for kp in fields
                               collect
                               (indent 2
                                       (declare-var :c++ (cadr kp) (car kp)))))))
              (if (struct-unnamed-p ty) "" ";"))))

  (defun struct-pre-var-c++ (ty)
    (let* ((ty-name (struct-spec-name ty))
           (is-unnamed (struct-unnamed-p ty))
           (pre-var (if is-unnamed
                        (translate-struct-c++ ty)
                        (c++-type-name ty-name)))
           (gvars (struct-spec-generic-vars ty)))
      (format nil "~a~a"
              pre-var
              (if (and gvars (not is-unnamed))
                  (format nil "< ~{~a~^, ~} >"
                          (mapcar (lambda (x) (translate :c++ x))
                                  gvars))
                  ""))))

  (defun declare-var-struct-c++ (ty vn)
    (let* ((pre-var (struct-pre-var-c++ ty)))
      (format nil "~a ~a;"
              pre-var
              (c++-var-name vn))))

  (defun define-struct-c++ (ty)
    (translate-struct-c++ ty))

  (defun get-keys (lst &optional (rest '()))
    "This function just gets every other element
  "
    (check-type lst (or cons null))
    (let ((head (car lst))
          (tail (cdr lst)))
      (case tail
        ((nil) (reverse rest))
        (otherwise (get-keys (cdr tail)
                             (cons head rest))))))

  (defun struct-check-type (ty cons-struct)
    (check-type cons-struct cons)
    (check-type ty (or struct-spec struct-identifier))
    (let* ((spec (struct-get-expanded-spec ty))
           (fields (struct-spec-fields spec)))
      (notany #'null
              (mapcar (lambda (key) (let ((type (assoc key fields)))
                                      (typep (getf cons-struct key)
                                             (getf type key))))
                      (get-keys cons-struct)))))

  (defun struct-caster-name (ty)
    (let* ((name (struct-spec-name ty))
           (spec (struct-get-expanded-spec ty))
           (gvars (struct-spec-generic-vars spec))
           (is-unnamed (struct-unnamed-p ty))
           (subtypes (mapcar #'cadr (struct-spec-fields spec)))
           (is-generic (struct-spec-generic-p spec)))
      (when is-generic (error "Cannot create a caster for generic struct"))
      ;(when is-unnamed (error "Can't create a caster name for unnamed structs"))
      (if gvars
          (format nil "s_~a_with_~{~a~^_and_~}"
              (c-var-name name)
              (mapcar (lambda (x) (caster-name :c++ x)) gvars))
          (format nil "s_~a" (c-var-name name)))))

  (defun struct-caster-body-of-unnamed-struct (spec parent-key
                                               &key (cl-object "o"))
    (format nil
            "{~&~{~&~a~^,~}~&} /* unnamed */"
            (struct-caster-body-from-subtypes
             spec
             :cl-object
             (format nil
                     "cl_getf(2, ~a, c_string_to_object(\"~s\"))"
                     cl-object
                     parent-key))))

  (defun struct-caster-body-from-subtypes (spec &key (cl-object "o"))
    (check-type spec struct-spec)
    (let* ((fields (struct-spec-fields spec))
           (subtypes (mapcar #'cadr fields))
           (format-arguments
             (mapcar (lambda (ty x y z) `(,ty
                                          ,x
                                          ,y
                                          ,z))
                     subtypes
                     (mapcar (lambda (x) (translate :c++ x)) subtypes)
                     (mapcar (lambda (x) (caster-name :c++ x)) subtypes)
                     (mapcar (lambda (x) (car x)) fields)
                     ))
           (is-generic (struct-spec-generic-p spec))
           (is-unnamed (struct-unnamed-p spec)))
      (mapcar (lambda (x)
                (cond
                  ;; Check for const unnamed structs
                  ((and (consp (car x))
                        (eq (caar x) 'const)
                        (typep (cadar x) '(or struct-identifier struct-spec))
                        (struct-unnamed-p (cadar x)))
                   (indent 2 (struct-caster-body-of-unnamed-struct
                              (cadar x) (cadddr x) :cl-object cl-object)))
                  ;; What happens if we have an unnamed struct??
                  ;; we can not really have a caster function
                  ;; in general for those
                  ((and (typep (car x) 'struct-spec)
                        (struct-unnamed-p (car x)))
                   (indent 2 (struct-caster-body-of-unnamed-struct
                              (car x) (cadddr x) :cl-object cl-object)))
                  ;; Regular types
                  (t (format
                      nil
                      (format
                       nil
                       "~?"
                       ;; v------ignore first x   v--- cl-object
                       "  ~**(~a*)~a(cl_getf(2, ~~a, c_string_to_object(\"~s\")))"
                       ;;          ^--caster name          struct key -----^
                       x)
                      cl-object))))
              format-arguments)
      ))

  (defun struct-caster-body (ty)
    (let* ((spec (struct-get-expanded-spec ty))
           (is-generic (struct-spec-generic-p spec))
           (constructor (struct-caster-body-from-subtypes spec)))
      (when is-generic (error "Cannot create a caster for generic struct"))
      (format nil "return (size_t)new ~a{~&~{~&~a~^,~}~&};"
              (struct-pre-var-c++ spec)
              constructor)))

  (defun struct-caster-header (ty)
    (let* ((subtypes (subtypes :c++ ty)))
      (format nil "~{~a~^~%~}"
              (mapcar (lambda (x)
                        (caster-signature :c++ x))
                      (remove-if #'struct-unnamed-p subtypes)))))

  )

(defmacro defgenericstruct (name spec)
  (let* ((spec `(struct ,name ,spec))
         (struct-name (struct-spec-name spec))
         (struct-spec-var (struct-spec-symbol struct-name)))
    `(progn
       (defparameter ,struct-spec-var ',spec))))

(defequiv :c++ (struct name)
  :translate (lambda (ty) (translate-struct-c++ ty))
  :declare-var (lambda (ty vn) (declare-var-struct-c++ ty vn))
  :define (lambda (ty) (define-struct-c++ ty))
  :subtypes #'struct-spec-subtypes
  :caster-name #'struct-caster-name
  :caster-header #'struct-caster-header
  :caster-body #'struct-caster-body)

;; unnamed structs
(defequiv :c++ (struct nil spec)
  :translate (lambda (ty) (translate-struct-c++ ty))
  :declare-var (lambda (ty vn) (declare-var-struct-c++ ty vn))
  :define (lambda (ty) (define-struct-c++ ty))
  :subtypes #'struct-spec-subtypes
  :caster-name #'struct-caster-name
  :caster-header #'struct-caster-header
  :caster-body #'struct-caster-body)

(deftype struct (name)
  `(and cons
        (satisfies ,(lambda (x)
                      (struct-check-type `(struct ,name) x)))))
(defun translate-enum-c++ (ty)
  (let* ((ty-name (cadr ty))
         (name (if ty-name (c++-type-name ty-name) ""))
         (fields (cddr ty)))
    (concatenate 'string
                 "enum "
                 (or name "")
                 " {"
                 (endl)
                 (eval
                  `(concatenate 'string
                                ,@(loop for kp in fields
                                        collect
                                        (format nil
                                                "  ~a,~a"
                                                kp
                                                (endl)))))
                 "}")))

(defun declare-var-enum-c++ (ty vn)
  (let* ((name (cadr ty))
         (fields (caddr ty))
         (pre-var (etypecase name
                    (null (translate-enum-c++ ty))
                    (t (string-capitalize name)))))
    (format nil "~a ~a;" pre-var (c++-var-name vn))))

(defun define-enum-c++ (ty)
  (format nil "~a;" (translate-enum-c++ ty)))

(defequiv :c++ (enum name args)
  :translate (lambda (ty) (translate-enum-c++ ty))
  :declare-var (lambda (ty vn) (declare-var-enum-c++ ty vn))
  :define (lambda (ty) (define-enum-c++ ty)))

(defequiv :c++ (member args)
  :translate (lambda (ty)
               (translate-enum-c++ `(enum nil ,@(cdr ty))))
  :declare-var (lambda (ty vn)
                 (declare-var-enum-c++ `(enum nil ,@(cdr ty)) vn))
  :define (lambda (ty)
            (define-enum-c++ `(enum nil ,@(cdr ty)))))
(defun struct-get-fields (s)
  (car s))
;;(defparameter *setting-spec-default* nil)
;;(eval-when (:compile-toplevel)
;;  (defun step-setting-spec-p (thing)
;;    (let ((ty (getf thing :type))
;;          (default (getf thing :default))
;;          (doc (getf thing :doc))
;;          (name (getf thing :name)))
;;      (check-type name keyword)
;;      (check-type doc string)
;;      (setq *setting-spec-default* default)
;;      ;; TODO: do this without setq
;;      (eval `(check-type *setting-spec-default* ,ty))
;;      (and name
;;           ty
;;           (member :default thing)
;;           (member :required thing)))))
;;
;;(deftype step-setting-spec ()
;;  '(satisfies step-setting-spec-p))
;;(eval-when (:compile-toplevel :load-toplevel)
;;  (defun consume-in-out (lst &optional (tail '()))
;;    (let ((first (car lst))
;;          (rest (cdr lst)))
;;      (cond
;;        ((eq first :out) `(,(reverse tail) ,rest))
;;        ((eq first :in) (consume-in-out rest tail))
;;        ((eq lst '()) `(,(reverse tail) ,rest))
;;        (t (consume-in-out rest (cons first tail)))))))
;;
;;
;;
;;(defun step-setting-typep (setting-pair setting-spec-list)
;;  (let* ((key (car setting-pair))
;;         (value (getf setting-pair key))
;;         (spec (car (remove-if-not (lambda (s)
;;                                     (eq key (getf s :name)))
;;                                   setting-spec-list))))
;;    (typep value (getf spec :type))))
;;
;;(defun step-setting-step-to-struct-spec (step-name args)
;;  " Input is
;;    'Tensor-Reader (:in setting-spec* :out setting-spec*)
;;  "
;;  (let* ((inout (consume-in-out args))
;;         (in-struct `(struct nil ,(mapcar
;;                                   (lambda (kp)
;;                                     `(,(getf kp :name)
;;                                       ,(getf kp :type)))
;;                                   (car inout))))
;;         (out-struct `(struct nil ,(mapcar
;;                                   (lambda (kp)
;;                                     `(,(getf kp :name)
;;                                       ,(getf kp :type)))
;;                                   (cadr inout)))))
;;    `(struct ,step-name ((:in ,in-struct)
;;                         (:out ,out-struct)))))
;;
;;(defun step-defequiv-c++ (step-name args)
;;  (let ((step-struct (step-setting-step-to-struct-spec step-name args)))
;;    (eval `(defequiv :c++ ,step-name
;;      :translate (lambda (ty) (translate :c++ ,step-struct))
;;      :declare-var (lambda (ty vn) (declare-var :c++ ,step-struct vn))
;;      :define (lambda (ty) (define :c++ ,step-struct))))))
;;
;;(defparameter *KIMIA-TYPES* '())
;;(defmacro defstep (name &rest args)
;;  ;; checking that name and args are of correct types
;;  (check-type name (and symbol (not keyword)))
;;  (let ((inout (consume-in-out args)))
;;    (dolist (in-or-out inout)
;;      (dolist (setting in-or-out)
;;        (check-type setting step-setting-spec))))
;;  (let* ((type-predicate-name (intern (format nil "~@:(~a~)-P" name)))
;;         (type-name (intern (format nil "~@:(~a~)" name)))
;;         (spec-fun-name (intern (format nil "~@:(~a~)-SPEC" name)))
;;         (default-type-fn (intern (format nil "~@:(~a~)-DEFAULT" name)))
;;         (inout (consume-in-out args))
;;         (in (car inout))
;;         (out (cadr inout)))
;;    `(progn
;;       (step-defequiv-c++ ',name ',args)
;;       (defun ,default-type-fn ()
;;         '(:name ,name
;;           :in ,(reduce (lambda (x y) (concatenate 'list x y))
;;                 (mapcar (lambda (s) `(,(getf s :name) ,(getf s :default)))
;;                  in))
;;           :out ,(reduce (lambda (x y) (concatenate 'list x y))
;;                  (mapcar (lambda (s) `(,(getf s :name) ,(getf s :default)))
;;                   out))))
;;       (defun ,spec-fun-name ()
;;         '(:name ,name :in ,in :out ,out))
;;       (defun ,type-predicate-name (thing)
;;         (check-type thing cons)
;;         (let* ((-name (getf thing :name))
;;                (-in (getf thing :in))
;;                (-in-keys (get-keys -in))
;;                (-out (getf thing :out))
;;                (-out-keys (get-keys -out))
;;                (spec (,spec-fun-name))
;;                (spec-name (getf spec :name))
;;                (spec-in (getf spec :in))
;;                (spec-out (getf spec :out)))
;;           (and (eq -name spec-name)
;;                (every (lambda (key)
;;                         (let* ((value (getf -in key))
;;                                (pair `(,key ,value)))
;;                           (step-setting-typep pair spec-in)))
;;                       -in-keys)
;;                (every (lambda (key)
;;                         (let* ((value (getf -out key))
;;                                (pair `(,key ,value)))
;;                           (step-setting-typep pair spec-out)))
;;                       -out-keys))))
;;       (push ',type-name *KIMIA-TYPES*)
;;       (deftype ,type-name ()
;;         '(satisfies ,type-predicate-name)))))
;;(defmacro check-step-type (step)
;;  (let ((name (getf step :name)))
;;    `(let ((step ',step))
;;       (check-type step ,name))))
;;(defmacro mk-stepq (name &rest args)
;;  (check-type name symbol)
;;  (let* ((in-out (consume-in-out args))
;;         (in (car in-out))
;;         (out (cadr in-out))
;;         (step `(:name ,name
;;                 :in ,in
;;                 :out ,out)))
;;    `(progn
;;       (check-step-type ,step)
;;       ',step
;;       )))
;;
;;(defun mk-step (name &rest args)
;;  (check-type name symbol)
;;  (let* ((in-out (consume-in-out args))
;;         (in (car in-out))
;;         (out (cadr in-out))
;;         (type)
;;         (step)
;;         )
;;    (setq type name)
;;    (setq step `(:name ,name
;;                 :in ,in
;;                 :out ,out))
;;    (eval `(check-step-type ,step))
;;    step))
(defparameter *KIMIA-STEPS* '())

(defmacro wrap-input-script (&rest arg)
  `(handler-bind
       ((error #'invoke-debugger))
     (in-package :kimia)
     (progn
       (format t "~%LISP::START evaluating KIMIA script~%")
       ,@arg
       (format t "~%LISP::DONE evaluating KIMIA~%")
       kimia::*KIMIA-STEPS*)))

;(defmacro >> (&rest args)
;  `(push (mk-stepq ,@args) *KIMIA-STEPS*))
(defun lines (str
              &optional (current-line '()) (rest '())
              &key (sep #\newline))
  (let* ((lst (coerce str 'list))
          (rest-str (coerce (cdr lst) 'string))
          (current-char (car lst)))
    (cond
      ((eq current-char sep)
        (lines rest-str
              '()
              (cons (coerce (reverse current-line) 'string) rest)
              :sep sep))
      ((null current-char)
        (reverse (cons (coerce (reverse current-line) 'string) rest)))
      (t (lines rest-str
                (cons current-char current-line)
                rest
                :sep sep)))))

(defun unlines (lst-str &key (sep "~%"))
  (format nil (format nil "~a~a~a" "~{~a~^" sep "~}") lst-str))

(defun words (str &key (sep #\space))
  (lines str nil nil :sep sep))

(defun unwords (str &key (sep #\space))
  (unlines str :sep sep))

(defun indent (n str &key (sep #\space))
  (format nil
          (format nil
                  "~~{~a~~a~a~~}"
                  (coerce (make-array n :initial-element sep) 'string)
                  "~%")
          (lines str)))

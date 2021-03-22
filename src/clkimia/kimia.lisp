(defpackage :kimia
  (:use :cl)
  (:nicknames :k))
(in-package :kimia)

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

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun plist-keys (lst &optional (rest '()))
    "This function just gets every other element"
    (check-type lst (or cons null))
    (let ((head (car lst))
          (tail (cdr lst)))
      (case tail
        ((nil) (reverse rest))
        (otherwise (plist-keys (cdr tail)
                             (cons head rest))))))

  (defun ulist-to-plist (lst keys
                         &key
                           (current-key nil)
                           (result nil)
                           (tail nil))
    (let ((first (car lst))
          (rest (cdr lst)))
      (cond
        ((member first keys) (ulist-to-plist rest keys
                                             :current-key first
                                             :result result))
        ((null lst) (let (ret)
                        (dolist (key (reverse keys))
                          (setf (getf ret key) (reverse (getf result key))))
                        ret))
        ((member current-key keys)
         (ulist-to-plist rest keys
                         :current-key current-key
                         :result (prog2 (push first (getf result current-key))
                                     result)))
        (t (ulist-to-plist rest keys
                           :current-key current-key
                           :result result)))))

  (defun consume-in-out (lst &optional (tail '()))
    (let ((first (car lst))
          (rest (cdr lst)))
      (cond
        ((eq first :out) `(,(reverse tail) ,rest))
        ((eq first :in) (consume-in-out rest tail))
        ((eq lst '()) `(,(reverse tail) ,rest))
        (t (consume-in-out rest (cons first tail)))))))


;; from http://cl-cookbook.sourceforge.net/strings.html
(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part
is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
          for old-pos = 0 then (+ pos part-length)
          for pos = (search part string
                            :start2 old-pos
                            :test test)
          do (write-string string out
                           :start old-pos
                           :end (or pos (length string)))
          when pos do (write-string replacement out)
            while pos)))
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
           (keys (plist-keys args)))
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
  :caster-body
  (lambda (ty)
    (format nil
  "~
const size_t dimension(o->base_string.dim)
           , charSize = ECL_EXTENDED_STRING_P(o) ? 4 : 1
           ;
std::string result;
ecl_base_char* c = o->base_string.self;
// TODO: handle the unicode well.
// right now I know it is 32bit characters,
// that is why the i * 4 is there
for (size_t i = 0; i < dimension; i++)
  result += *(c + i * charSize);
return (size_t)new std::string(result);"))
  :caster-name "clstr")
(defparameter +c++-vector-body+
"~
~a result~a;
for (size_t i=0; i < result.size(); i++) {
  cl_object index(c_string_to_object(std::to_string(i).c_str()));
  ~a *element = (~a*)~a(cl_aref(2, o, index));
  result[i] = *element;
}
return (size_t)new ~a(result);")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun vector-body-c++ (ty &key (array nil))
    (format nil
            +c++-vector-body+
            (translate :c++ ty)
            (if array "" "(ecl_to_int(cl_length(o)))")
            (translate :c++ (cadr ty))
            (translate :c++ (cadr ty))
            (caster-name :c++ (cadr ty))
            (translate :c++ ty))))


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

  :caster-body #'vector-body-c++)

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
    (vector-body-c++ ty :array t)))


;; generic variables
(defequiv :c++ (G N)
  :translate (lambda (ty)
               (format nil "_G~a" (cadr ty))))

(defun generic-p (type)
  (etypecase type
    (cons (eq 'g (car type)))
    (t nil)))
(defun pointer-p (ty ps)
  (if (symbolp ps)
      (if (boundp ps)
          (typep (eval ps) ty)
          t)
      (typep ps ty)))

(deftype pointer (type-pointed-to)
  `(satisfies ,(lambda (x) (pointer-p type-pointed-to x))))

(defequiv :c++ (pointer F)
  :translate (lambda (ty) (format nil "~a*" (translate :c++ (cadr ty))))
  :caster-body
  (lambda (ty)
    (format nil
"~
bool isSymbol = ecl_to_bool(cl_symbolp(o));
auto name
  = isSymbol
  ? (std::string*)~a(cl_string(o))
  : (std::string*)~:*~a(cl_string(cl_gensym(0)))
  ;
// it is an immediate value, so return the pointer to its pointer
if (!isSymbol) {
  POINTER_DATABASE[*name] = (size_t)new size_t(~a(o));
  return POINTER_DATABASE[*name];
}
// It is a symbol, so we have to check in the database
// if the symbol is already registered there
if (POINTER_DATABASE.find(*name) != POINTER_DATABASE.end()) {
  return POINTER_DATABASE[*name];
}
bool isBound = ecl_to_bool(cl_boundp(o));
// It is not in the database, we have to check if the symbol
// is bound or unbound
if (isBound) {
  //      init from value in pointer ---------v
  POINTER_DATABASE[*name] = (size_t)new size_t(~:*~a(cl_eval(o)));
} else {
  // assume there is a default constructor
  size_t i = (size_t)new ~a();
  POINTER_DATABASE[*name] = (size_t)new size_t(i);
}
return POINTER_DATABASE[*name];"
(caster-name :c++ 'string)
(caster-name :c++ (cadr ty))
(translate :c++ (cadr ty))))
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
         (typep (cadr ty) '(or cons symbol)) ;; name
         (typep (caddr ty) '(or cons null))  ;; fields container
         (every (lambda (x) (typep x '(or cons null))) (caddr ty))
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

  (defun struct/get-ungeneric-name (ty)
    (if (struct/name-generic-p ty)
        (caadr ty)
        (cadr ty)))

  (defun struct/get-generic-name (ty)
    (check-type ty (or struct-spec struct-identifier))
    (let* ((spec (struct-get-spec ty))
           (is-unnamed (struct-unnamed-p ty))
           (is-generic (struct/name-generic-p spec))
           (gvars (struct-spec-generic-vars spec))
           (safe-gvars (mapcar (lambda (g)
                                 (if (generic-p g)
                                     g
                                     `(g ,g)))
                               gvars))
           (subst-list (pairlis safe-gvars gvars)))
      (if (or is-unnamed (not is-generic))
          ty
          `(struct (,(struct/get-ungeneric-name ty)
                    ,@safe-gvars)))))

  (defun struct/name-generic-p (ty)
    (check-type ty (or struct-spec struct-identifier))
    (typep (cadr ty) 'cons))

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
                               (indent
                                2
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

  (defun struct/check-type (cons-struct ty)
    (check-type cons-struct cons)
    (check-type ty (or struct-spec struct-identifier))
    (let* ((spec (struct-get-expanded-spec ty))
           (fields (struct-spec-fields spec)))
      (notany #'null
              (mapcar (lambda (key) (let ((type (assoc key fields)))
                                      (typep (getf cons-struct key)
                                             (getf type key))))
                      (plist-keys cons-struct)))))

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
    (let* ((spec (struct-get-expanded-spec ty))
           (subtypes (subtypes :c++ spec))
           (subtypes-no-unnamed (remove-if #'struct-unnamed-p subtypes)))
      (format nil "~{~a~^~%~}"
              (mapcar (lambda (x) (caster-signature :c++ x))
                      subtypes-no-unnamed))))

  )

(defmacro defstruct! (name spec)
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

(deftype struct (name &optional (fields nil))
  `(and (or null cons)
        (satisfies ,(lambda (x)
                      (struct/check-type x `(struct ,name ,fields))))))
(deftype choice (type elements)
  `(and ,type
    (satisfies ,(lambda (x) (typep elements 'cons)))
    (satisfies ,(lambda (x) (every (lambda (x) (typep x type)) elements)))
    (satisfies ,(lambda (x) (member x elements :test #'equal)))))

(defequiv :c++ (choice type elements)
  :translate (lambda (ty) (translate :c++ (cadr ty)))
  :caster-name (lambda (ty) (caster-name :c++ (cadr ty)))
  :subtypes (lambda (ty) `(,(cadr ty)))
  :caster-header (lambda (ty) (caster-signature :c++ (cadr ty)))
  :caster-body (lambda (ty) (caster-body :c++ (cadr ty))))
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
(defun step-setting-spec-p (thing)
  (let ((ty (getf thing :type))
        (default (getf thing :default))
        (doc (getf thing :doc))
        (name (getf thing :name)))
    (unless (typep name 'keyword)
      (error "The name of the spec setting should be a keyword"))
    (when default
      (unless (typep default ty)
        (error (format nil "Default value ~s should be of type ~s"
                       default ty))))
    (check-type doc string)
    (assert (not (string-equal doc ""))) ;; it only takes 5 seconds
    (and name
         ty
         (member :doc thing))))

(deftype step-setting-spec ()
  '(and cons
    (satisfies step-setting-spec-p)))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun defstep-keywords ()
    '(:in :out :run)))
(defun step-setting-typep (setting-pair setting-spec-list)
  (let* ((key (car setting-pair))
         (value (getf setting-pair key))
         (spec (find key setting-spec-list :key (lambda (k) (getf k :name)))))
    (typep value (getf spec :type))))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun step/spec-to-struct-spec (step-name in out)
    (let ((in-struct `(struct nil
                              ,(mapcar
                                (lambda (kp)
                                  `(,(getf kp :name)
                                    ,(getf kp :type)))
                                in)))
          (out-struct `(struct nil
                               ,(mapcar
                                 (lambda (kp)
                                   `(,(getf kp :name)
                                     ,(getf kp :type)))
                                 out))))
      `(struct ,step-name ((:in (const ,in-struct))
                           (:out ,out-struct))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun step/get-name (generic-name)
    (etypecase generic-name
      (symbol generic-name)
      (cons (car generic-name))))

  (defun step/get-spec-symbol (name)
    (intern (format nil "~@:(~a-step-spec~)" (step/get-name name))))
  (defun step/get-spec (name)
    (eval (step/get-spec-symbol name))))
(defun step/get-struct-spec (step-name)
  (let ((struct-identifier `(struct ,step-name)))
    (struct-get-spec struct-identifier)))

(defun step/get-struct-expanded-spec (step-name)
  (let ((struct-identifier `(struct ,step-name)))
    (struct-get-expanded-spec struct-identifier)))

(defun step/check-type (thing step-name)
  (let ((spec (step/get-struct-expanded-spec step-name)))
    (eval `(check-type ',thing ,spec))))

(defun step/is-generic (step-name)
  (let ((spec (step/get-struct-expanded-spec step-name)))
    (struct-spec-generic-p spec)))

(defun step/translate-struct (lang step-name)
  (let* ((spec (step/get-struct-spec step-name))
         (gvars (struct-spec-generic-vars spec))
         (safe-gvars (mapcar (lambda (g)
                               (if (generic-p g)
                                   g `(g ,g))) gvars))
         (generic-name (struct/get-generic-name spec)))
    (translate lang generic-name)
    ))

;; todo generalize out of c++
(defun step/run-function-name (lang name)
  (flet ((fun-format (fname) (etypecase fname
                               (symbol (c++-var-name fname))
                               (string fname))))
    (let* ((spec (step/get-spec name))
           (run (getf spec :run)))
      (etypecase run
        (cons (format nil "~a<~{~a~^, ~}>"
                      (fun-format (car run))
                      (mapcar (lambda (x) (translate lang x))
                              (cdr name))))
        ((or string symbol) (fun-format run))))))

(defparameter *KIMIA-STEP-SPECS* '()
  "List of all defined step specs in the current session.")

(defparameter *KIMIA-STEP-INSTANTIATIONS* '()
  "List of all instantiated steps in the current session.")

(defun step/instantiate (step-identifier)
  (push step-identifier *kimia-step-instantiations*))

(defmacro defstep (name &rest args)
  (check-type name (or symbol cons))
  (let* ((step-name (step/get-name name))
         (type-predicate-name (intern (format nil "~@:(~a~)-P" step-name)))
         (spec-var-name (step/get-spec-symbol step-name))
         (default-type-fn (intern (format nil "~@:(~a~)-DEFAULT" step-name)))
         (ulist (ulist-to-plist args (defstep-keywords)))
         (run (car (getf ulist :run)))
         (in (getf ulist :in))
         (out (getf ulist :out)))
    (unless run (error "Please provide a run function name in a :run field."))
    (assert (equal (type-of run) (type-of name)))
    `(progn
       (defstruct! ,name ,(caddr (step/spec-to-struct-spec name in out)))
       (setf ,spec-var-name '(:name ,name
                              :in ,in
                              :out ,out
                              :run ,run))
       (push ',spec-var-name *KIMIA-STEP-SPECS*))))
(defun step/caster-name (lang name)
  (caster-name lang `(struct ,name)))

(defun make-step (name &rest args)
  (check-type name (or cons symbol))
  (let* ((ulist (ulist-to-plist args (defstep-keywords)))
         (in (getf ulist :in))
         (out (getf ulist :out))
         (run (getf ulist :run))
         (step `(:name ,name
                 ;:caster-name-c++ ,(step/caster-name :c++ name)
                 :run-name-c++ ,(step/run-function-name :c++ name)
                 :struct (:in ,in
                          :out ,out))))
    (eval `(step/check-type ',(getf step :struct) ',name))
    step))
;;
(defparameter *KIMIA-STEPS* '())

(defmacro wrap-input-script (&rest arg)
  `(handler-bind
       ((error #'invoke-debugger))
     (in-package :kimia)
     (progn
       (format t "~%LISP::START evaluating KIMIA script~%")
       ,@arg
       (format t "~%LISP::DONE evaluating KIMIA~%")
       (reverse kimia::*KIMIA-STEPS*))))

(defmacro $ (&rest args)
  (let ((step (eval `(make-step ,@args))))
        `(push ',step *kimia-steps*)))

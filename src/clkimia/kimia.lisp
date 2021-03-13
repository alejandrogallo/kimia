(defpackage :kimia.types
  (:use :cl)
  (:nicknames :kt))
(in-package :kimia.types)

(defun endl () (format nil "~%"))

(defun c++-type-name (thing)
  (remove-if (lambda (x) (string= x "-"))
             (string-capitalize
              (string-downcase thing))))

(defun c++-var-name (thing)
  (nstring-downcase
   (remove-if (lambda (x) (string= x "-"))
              (string-capitalize
               (string-downcase thing)))
   :start 0
   :end 1))

(defparameter *KIMIA-TYPES* '())

(defmacro c++-declare-var-fn-default (translate)
  `(lambda (ty vn)
     (format nil "~a ~a;"
             (funcall ,translate ty)
             (c++-var-name vn))))


(defmacro deftype-c++ (type &key
                              translate
                              (declare-var nil)
                              (define nil)
                              (satisfies nil)
                              (generic nil))
  (let* ((type-name (etypecase type
                      (cons (car type))
                      (symbol type)))
         (type-c++-name (intern (format nil "~A-C++" type-name)))
         (fun-or-scalar (lambda (thing)
                          (etypecase thing
                            (cons (eval thing))
                            (compiled-function thing)
                            ((or null string) (eval `(lambda (&optional args)
                                                       ,thing))))))
         (translate (funcall fun-or-scalar translate))
         (declare-var (or declare-var
                          (c++-declare-var-fn-default translate))))
    `(progn
       (defparameter ,type-c++-name nil)
       (setq ,type-c++-name
             '(:translate ,translate
               :declare-var ,(funcall fun-or-scalar declare-var)
               :define ,(funcall fun-or-scalar define))))
    ))

(defmacro translate-c++ (ty)
  (let* ((ty-name (etypecase ty
                    (cons (car ty))
                    (symbol ty)))
         (ty-c++ (intern (format nil "~a-C++" ty-name))))
    `(funcall (getf ,ty-c++ :translate) ',ty)))

(defmacro define-c++ (ty)
  (let* ((ty-name (etypecase ty
                    (cons (car ty))
                    (symbol ty)))
         (ty-c++ (intern (format nil "~a-C++" ty-name))))
    `(funcall (getf ,ty-c++ :define) ',ty)))

(defmacro declare-var-c++ (ty vn)
  (let* ((ty-name (etypecase ty
                    (cons (car ty))
                    (symbol ty)))
         (ty-c++ (intern (format nil "~a-C++" ty-name))))
    `(funcall (getf ,ty-c++ :declare-var) ',ty ',vn)))
(deftype-c++ integer :translate "int")
(deftype-c++ double :translate "double")
(deftype-c++ string :translate "std::string")
(deftype-c++ double-float :translate "double")
(deftype-c++ single-float :translate "float")
(deftype-c++ float :translate "float")
(deftype-c++ boolean :translate "bool")

(deftype-c++ (vector F)
  :translate (lambda (ty)
               (format nil "std::vector< ~a >"
                       (eval `(translate-c++ ,(cadr ty))))))

(deftype-c++ (array F N)
  :translate (lambda (ty)
               (format nil "std::array< ~a, ~a >"
                       (eval `(translate-c++ ,(cadr ty)))
                       (caddr ty))))
(defun translate-struct-c++ (ty)
  (let* ((ty-name (cadr ty))
         (name (if ty-name (c++-type-name ty-name) ""))
         (fields (caddr ty)))
    (concatenate 'string
                 "struct " (or name "")
                 " {" (endl)
                 (eval
                  `(concatenate 'string
                                ,@(loop for kp in fields
                                        collect
                                        (format nil
                                                "  ~a~a"
                                                (eval
                                                 `(declare-var-c++
                                                   ,(cadr kp)
                                                   ,(car kp)))
                                                (endl)))))
                 "}")))

(defun declare-var-struct-c++ (ty vn)
  (let* ((name (cadr ty))
         (pre-var (etypecase name
                    (null (translate-struct-c++ ty))
                    (t (c++-type-name name)))))
    (format nil "~a ~a;"
            pre-var
            (c++-var-name vn))))

(defun define-struct-c++ (ty)
  (format nil "~a;" (translate-struct-c++ ty)))

(deftype-c++ (struct name args)
  :translate (lambda (ty) (translate-struct-c++ ty))
  :declare-var (lambda (ty vn) (declare-var-struct-c++ ty vn))
  :define (lambda (ty) (define-struct-c++ ty)))

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

(deftype-c++ (enum name args)
  :translate (lambda (ty) (translate-enum-c++ ty))
  :declare-var (lambda (ty vn) (declare-var-enum-c++ ty vn))
  :define (lambda (ty) (define-enum-c++ ty)))

(deftype-c++ (member args)
  :translate (lambda (ty)
               (translate-enum-c++ `(enum nil ,@(cdr ty))))
  :declare-var (lambda (ty vn)
                 (declare-var-enum-c++ `(enum nil ,@(cdr ty)) vn))
  :define (lambda (ty)
            (define-enum-c++ `(enum nil ,@(cdr ty)))))
(defparameter *setting-spec-default* nil)
(eval-when (:compile-toplevel)
  (defun step-setting-spec-p (thing)
    (let ((ty (getf thing :type))
          (default (getf thing :default))
          (doc (getf thing :doc))
          (name (getf thing :name)))
      (check-type name keyword)
      (check-type doc string)
      (setq *setting-spec-default* default)
      ;; TODO: do this without setq
      (eval `(check-type *setting-spec-default* ,ty))
      (and name
           ty
           (member :default thing)
           (member :required thing)))))

(deftype step-setting-spec ()
  '(satisfies step-setting-spec-p))
(eval-when (:compile-toplevel :load-toplevel)
  (defun consume-in-out (lst &optional (tail '()))
    (let ((first (car lst))
          (rest (cdr lst)))
      (cond
        ((eq first :out) `(,(reverse tail) ,rest))
        ((eq first :in) (consume-in-out rest tail))
        ((eq lst '()) `(,(reverse tail) ,rest))
        (t (consume-in-out rest (cons first tail)))))))

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

(defun step-setting-typep (setting-pair setting-spec-list)
  (let* ((key (car setting-pair))
         (value (getf setting-pair key))
         (spec (car (remove-if-not (lambda (s)
                                     (eq key (getf s :name)))
                                   setting-spec-list))))
    (typep value (getf spec :type))))

(defun step-setting-step-to-struct-spec (step-name args)
  " Input is
    'Tensor-Reader (:in setting-spec* :out setting-spec*)
  "
  (let* ((inout (consume-in-out args))
         (in-struct `(struct nil ,(mapcar
                                   (lambda (kp)
                                     `(,(getf kp :name)
                                       ,(getf kp :type)))
                                   (car inout))))
         (out-struct `(struct nil ,(mapcar
                                   (lambda (kp)
                                     `(,(getf kp :name)
                                       ,(getf kp :type)))
                                   (cadr inout)))))
    `(struct ,step-name ((:in ,in-struct)
                         (:out ,out-struct)))))

(defun step-deftype-c++ (step-name args)
  (let ((step-struct (step-setting-step-to-struct-spec step-name args)))
    (eval `(deftype-c++ ,step-name
      :translate (lambda (ty) (translate-c++ ,step-struct))
      :declare-var (lambda (ty vn) (declare-var-c++ ,step-struct vn))
      :define (lambda (ty) (define-c++ ,step-struct))))))

(defparameter *KIMIA-TYPES* '())
(defmacro defstep (name &rest args)
  ;; checking that name and args are of correct types
  (check-type name (and symbol (not keyword)))
  (let ((inout (consume-in-out args)))
    (dolist (in-or-out inout)
      (dolist (setting in-or-out)
        (check-type setting step-setting-spec))))
  (let* ((type-predicate-name (intern (format nil "~@:(~a~)-P" name)))
         (type-name (intern (format nil "~@:(~a~)" name)))
         (spec-fun-name (intern (format nil "~@:(~a~)-SPEC" name)))
         (default-type-fn (intern (format nil "~@:(~a~)-DEFAULT" name)))
         (inout (consume-in-out args))
         (in (car inout))
         (out (cadr inout)))
    `(progn
       (step-deftype-c++ ',name ',args)
       (defun ,default-type-fn ()
         '(:name ,name
           :in ,(reduce (lambda (x y) (concatenate 'list x y))
                 (mapcar (lambda (s) `(,(getf s :name) ,(getf s :default)))
                  in))
           :out ,(reduce (lambda (x y) (concatenate 'list x y))
                  (mapcar (lambda (s) `(,(getf s :name) ,(getf s :default)))
                   out))))
       (defun ,spec-fun-name ()
         '(:name ,name :in ,in :out ,out))
       (defun ,type-predicate-name (thing)
         (check-type thing cons)
         (let* ((-name (getf thing :name))
                (-in (getf thing :in))
                (-in-keys (get-keys -in))
                (-out (getf thing :out))
                (-out-keys (get-keys -out))
                (spec (,spec-fun-name))
                (spec-name (getf spec :name))
                (spec-in (getf spec :in))
                (spec-out (getf spec :out)))
           (and (eq -name spec-name)
                (every (lambda (key)
                         (let* ((value (getf -in key))
                                (pair `(,key ,value)))
                           (step-setting-typep pair spec-in)))
                       -in-keys)
                (every (lambda (key)
                         (let* ((value (getf -out key))
                                (pair `(,key ,value)))
                           (step-setting-typep pair spec-out)))
                       -out-keys))))
       (push ',type-name *KIMIA-TYPES*)
       (deftype ,type-name ()
         '(satisfies ,type-predicate-name)))))
(defmacro check-step-type (step)
  (let ((name (getf step :name)))
    `(let ((step ',step))
       (check-type step ,name))))
(defmacro mk-stepq (name &rest args)
  (check-type name symbol)
  (let* ((in-out (consume-in-out args))
         (in (car in-out))
         (out (cadr in-out))
         (step `(:name ,name
                 :in ,in
                 :out ,out)))
    `(progn
       (check-step-type ,step)
       ',step
       )))

(defun mk-step (name &rest args)
  (check-type name symbol)
  (let* ((in-out (consume-in-out args))
         (in (car in-out))
         (out (cadr in-out))
         (type)
         (step)
         )
    (setq type name)
    (setq step `(:name ,name
                 :in ,in
                 :out ,out))
    (eval `(check-step-type ,step))
    step))

(defpackage :kimia
  (:use :cl)
  (:nicknames :k))
(in-package :kimia)

(defparameter *KIMIA-STEPS* '())

(defmacro wrap-input-script (&rest arg)
  `(handler-bind
       ((error #'invoke-debugger))
       (progn
         (in-package :kimia)
         (format t "~%LISP::START evaluating KIMIA script~%")
         ,@arg
         (format t "~%LISP::DONE evaluating KIMIA~%")
         *KIMIA-STEPS*)))

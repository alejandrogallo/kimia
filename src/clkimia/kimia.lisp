(defpackage :kimia.types
  (:use :cl)
  (:nicknames :kt))
(in-package :kimia.types)

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
         (member :required thing))))

(deftype step-setting-spec ()
  '(satisfies step-setting-spec-p))
(defun consume-in-out (lst &optional (tail '()))
  (let ((first (car lst))
        (rest (cdr lst)))
    (cond
      ((eq first :out) `(,(reverse tail) ,rest))
      ((eq first :in) (consume-in-out rest tail))
      ((eq lst '()) `(,(reverse tail) ,rest))
      (t (consume-in-out rest (cons first tail))))))

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
         (c++-name-fn (intern (format nil "~@:(~a~)-C++-NAME" name)))
         (inout (consume-in-out args))
         (in (car inout))
         (in-keys (mapcar (lambda (x) (getf x :name)) in))
         (out (cadr inout)))
    `(progn
       (defun ,c++-name-fn ()
         ,(remove-if (lambda (x) (string= x "-"))
                    (string-capitalize
                     (string-downcase (string name)))))
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

(defmacro assert-not (thing)
  `(let ((value (not ,thing)))
     (unless value
       (princ ',thing)
       (error "Assertion-not error"))))

(defmacro assert-equal (one two)
  `(let ((value (equal ,one ,two)))
     (unless value
       (format t "~2%~s~%~Tis not equal to ~%~T~s~2%"
               ',one ',two)
       (assert nil))))

(defmacro assert-eq (one two)
  `(let ((value (eq ,one ,two)))
     (unless value
       (format t "~2%~s~%~Tis not eq to ~%~T~s~2%"
               ',one ',two)
       (assert nil))))

(in-package :kimia)
(load "test-data.lisp")


(defmacro assert-not (thing)
  `(let ((value (not ,thing)))
     (unless value
       (princ ',thing)
       (error "Assertion-not error"))))

(defmacro assert-equal (one two)
  `(let ((value (equal ,one ,two)))
     (unless value
       (format t "~2%~s~%~Tis not equal to ~%~T~s~2%"
               ',one ',two)
       (assert nil))))

(defmacro assert-eq (one two)
  `(let ((value (eq ,one ,two)))
     (unless value
       (format t "~2%~s~%~Tis not eq to ~%~T~s~2%"
               ',one ',two)
       (assert nil))))
(assert-equal (words "hello world")
              '("hello" "world"))
(assert-equal (words "hello  world")
              '("hello" "" "world"))
(assert-equal (words "tensor.lens.pphh" :sep #\.)
              '("tensor" "lens" "pphh"))
(assert-equal (unwords '("tensor" "lens" "pphh") :sep #\.)
              "tensor.lens.pphh")
(assert-equal (unlines '("tensor" "lens" "pphh"))
              (format nil "tensor~%lens~%pphh"))
(assert-equal (lines (format nil "tensor~%lens~%pphh"))
              '("tensor" "lens" "pphh"))

;; plist-keys
(assert-equal (plist-keys '(:asdf 5 :err 98))
              '(:asdf :err))
(assert-equal (plist-keys '(:asdf 5 :err))
              '(:asdf))

;; ulist-to-plist
(assert-equal (ulist-to-plist '(:in 654 9 8 :key :word
                                :out this and that and well
                                :fun 5 6)
                              '(:in :out :fun))
              '(:in (654 9 8 :key :word)
                :out (this and that and well)
                :fun (5 6)))
(assert-equal (ulist-to-plist '(:in 654 9 8 :key :word
                                :out this and that and well
                                :fun 5 6)
                              '(:missing))
              '(:missing nil))
(assert-equal (ulist-to-plist '(:in 654 9 8 :key :word) '())
              '())
(assert-equal (ulist-to-plist '(:in (:la 654) (:lo 9) (:lu 8) (:li :key)
                                :out (this) (and) (that) (and) (well)
                                :fun 5 6)
                              '(:in :out :fun))
              '(:IN ((:LA 654) (:LO 9) (:LU 8) (:LI :KEY)) :OUT
                ((THIS) (AND) (THAT) (AND) (WELL)) :FUN (5 6)))
(assert (string= (c++-type-name 'tensor-reader) "TensorReader"))
(assert (string= (c++-type-name "TeNsor-ReAder") "TensorReader"))
(assert (string= (c-type-name "TeNsor-ReAder") "tensor_reader_t"))
(assert (string= (c++-var-name "TeNsor-ReAder") "tensorReader"))
(assert (string= (c-var-name "TeNsor-ReAder") "tensor_reader"))
(dolist (lang '(:c :c++))
  ;; caster-signature-fmt
  (assert-equal (caster-signature-fmt lang) "~&size_t ~a (const cl_object o)")
  ;; caster-envelope-fmt
  (assert-equal (caster-envelope-fmt lang)
                "~&~a~&size_t ~a (const cl_object o){~&~a~&}"))

;; internal-type-name
(assert-equal (internal-type-name 'integer)
              "integer")
(assert-equal (internal-type-name '(struct something asdf ))
              "struct-3")

;; defequiv-var-name
(assert-eq (defequiv-var-name :c '(struct something asdf ))
           'STRUCT-3-C)
(assert-eq (defequiv-var-name :c++ '(struct something asdf ))
           'STRUCT-3-C++)

;; defequiv-var-name
(assert-eq (defequiv-var-name :c++ '(vec F N))
           'vec-3-c++)
(assert-eq (defequiv-var-name :c '(struct name (vars) (fields)))
           'struct-4-c)
(assert-eq (defequiv-var-name :c '(struct name (vars) (fields)))
           'struct-4-c)
(assert-eq (defequiv-var-name :c++ 'integer)
           'integer-c++)
(assert-eq (defequiv-var-name :c 'integer)
           'integer-c)
(assert-equal (translate :c++ 'integer) "int")
(assert-equal (translate :c++ 'double-float) "double")
(assert-equal (translate :c++ 'single-float) "float")
(assert-equal (translate :c++ 'boolean) "bool")
(assert-equal (translate :c++ 'string) "std::string")

(assert-equal (declare-var :c++ 'integer 'this-is-a-variable)
              "int thisIsAVariable;")

(assert-equal (declare-var :c++ 'integer 'this-is-a-variable)
              "int thisIsAVariable;")

(assert-equal (caster-snippet :c++ 'double-float)
"size_t cldouble (const cl_object o){
  return (size_t)new double(ecl_to_double(o));
}")

(assert-equal (translate :c++ '(vec double-float))
              "std::vector< double >")
(assert-equal (translate :c++ '(vec (g 5)))
              "std::vector< _G5 >")
(assert-equal (translate :c++ '(vec (g 5) 8))
              "std::array< _G5, 8 >")
(assert-equal (translate :c++ '(vec (vec (vec double-float) 2) 8))
              "std::array< std::array< std::vector< double >, 2 >, 8 >")

;;; CASTING
(assert-equal (caster-name :c++ '(vec integer 8))
              "ar_of_8_clint")

(assert-equal (caster-snippet :c++ '(vec integer))
"size_t clint (const cl_object o);
size_t v_of_clint (const cl_object o){
  std::vector< int > result(ecl_to_int(cl_length(o)));
  for (size_t i=0; i < result.size(); i++) {
    cl_object index(c_string_to_object(std::to_string(i).c_str()));
    int *element = (int*)clint(cl_aref(2, o, index));
    result[i] = *element;
  }
  return (size_t)new std::vector< int >(result);
}")

(assert-equal (caster-name :c++ '(vec (vec double-float) 8))
              "ar_of_8_v_of_cldouble")

(assert-equal (caster-snippet :c++ '(vec (vec double-float) 8))
"size_t ar_of_8_v_of_cldouble (const cl_object o){
  std::array< std::vector< double >, 8 > result;
  for (size_t i=0; i < result.size(); i++) {
    cl_object index(c_string_to_object(std::to_string(i).c_str()));
    std::vector< double > *element = (std::vector< double >*)v_of_cldouble(cl_aref(2, o, index));
    result[i] = *element;
  }
  return (size_t)new std::array< std::vector< double >, 8 >(result);
}")
(assert (generic-p '(g 5)))
(assert (generic-p '(g a)))
(assert (not (generic-p '(vec F))))
(assert-equal (translate :c++ '(g a))
              "_GA")
(assert-equal (translate :c++ '(g 98))
              "_G98")
;; undbound symbols are pointers to anything
(assert (typep (gensym) '(pointer integer)))
(assert (typep (gensym) '(pointer lala)))
(defparameter *test-mypointer* 5)
(let ((myint 5898))
  (check-type myint
              (pointer integer))
  (check-type *test-mypointer*
              (pointer integer))
  (assert (typep *test-mypointer* '(pointer integer)))
  (assert (typep 5 '(pointer integer)))
  (assert-not (typep 5.5 '(pointer integer)))
  (assert-not (typep 5.5d0 '(pointer integer)))
  (assert (typep 5.5d0 '(pointer double-float)))
  (assert-not (typep 5.5 '(pointer double-float)))
  (assert (typep myint '(pointer integer)))
  (let ((*test-mypointer* 5.5))
    (assert-not (typep *test-mypointer*
                       '(pointer integer)))))

(assert-equal (translate :c++ '(pointer integer))
              "int*")
(assert-equal (translate :c++ '(pointer (vec (pointer (pointer integer)))))
              "std::vector< int** >*")
(assert (typep 5 '(const integer)))
(assert-not (typep 5.5 '(const integer)))

(assert-equal (translate :c++ '(const integer)) "const int")
(assert-equal (translate :c++ '(const (vec (pointer (const integer)))))
              "const std::vector< const int* >")
;; the casters should be the same really
(assert-equal (caster-snippet :c++ '(const double-float))
"size_t cldouble (const cl_object o);
size_t ccldouble (const cl_object o){
  return (size_t)new double(ecl_to_double(o));
}")
(assert tensor-reader-double-spec)
(check-type tensor-reader-double-spec struct-spec)

;; struct-spec-name ;;;;;;;;;;;;;;;
(assert-eq (struct-spec-name tensor-reader-double-spec)
           'tensor-reader-double)
(assert-eq (struct-spec-name '(struct tensor-reader-double))
           'tensor-reader-double)
;; spec
(assert-eq (struct-spec-name '(struct (tensor-reader-double A F) ((:some type))))
           'tensor-reader-double)
;; identifier
(assert-eq (struct-spec-name '(struct (tensor-reader-double A F)))
           'tensor-reader-double)
;; unnammed
(assert-eq (struct-spec-name '(struct (nil A F)))
           nil)

;; SPEC FIELDS ;;;;;;;;;;;
(assert-equal (struct-spec-fields (eval (struct-spec-symbol
                                         'tensor-reader-double)))
              (caddr tensor-reader-double-spec))

;; template line

(assert-equal (struct-template-line '(struct tensor-reader-double))
              "")
(assert-equal (struct-template-line '(struct (tensor-reader-g integer)))
              "template")
(assert-equal (struct-template-line '(struct (tensor-reader-g (g 5))))
              "template < typename _G5 >")
(assert-equal (struct-template-line '(struct
                                      (davidson-solver
                                       (g 1) (g 2) (g 3) (g 4))))
              "template < typename _G1, typename _G2, typename _G3, typename _G4 >")

;;; get spec
(assert-equal (struct-get-spec '(struct (davidson-solver F G H A)))
              davidson-solver-spec)

;; struct-spec-generic-vars
(assert-equal (struct-spec-generic-vars '(struct (davidson-solver
                                                  (g 1) (g 2) (g 3) (g 4))))
              '((g 1) (g 2) (g 3) (g 4)))
(assert-equal (struct-spec-generic-vars '(struct (davidson-solver
                                                  integer double lala F)))
              '(integer double lala F))

;; struct-spec-generic-p
(assert (struct-spec-generic-p
         '(struct (davidson-solver (g 1) (g 2) (g 3) (g 4)))))
(assert (struct-spec-generic-p
         '(struct (davidson-solver integer (g 2) (g 3) (g 4)))))
(assert (struct-spec-generic-p
         '(struct (davidson-solver integer string (g 3) (g 4)))))
(assert (struct-spec-generic-p
         '(struct (davidson-solver integer string integer (g 4)))))
(assert-not (struct-spec-generic-p
             '(struct (davidson-solver integer string integer integer))))


(struct-get-expanded-spec '(struct tensor-reader-double))

;; TYPE CHECKING
(assert (struct/check-type '(:name "hello world" :lens #(5.0d0 9.0d0))
                           '(struct tensor-reader-double)))
(assert-not (struct/check-type '(:name "hello world" :lens #(5.0d0 9.0))
                               '(struct tensor-reader-double)))
(assert-not (struct/check-type '(:name 5 :lens #(5.0d0 9.0d0))
                               '(struct tensor-reader-double)))

;; unnamed structs
(assert (typep '(struct nil ((:name string))) 'struct-spec))
(assert (typep '(struct nil nil) 'struct-spec))
(assert-equal (struct-get-expanded-spec '(STRUCT NIL ((:name string))))
              '(STRUCT NIL ((:name string))))
(assert (struct/check-type '(:name "some string")
                           '(STRUCT NIL ((:name string)))))

(assert (typep '(:name 654.5d0)
               '(struct (Uttu double-float))))
(assert-not (typep '(:name 654.5d0)
                   '(struct (Uttu integer))))

(assert (typep '(struct (Uttu integer))
               'struct-identifier))
(assert-not (typep '(struct (Uttu integer))
                   'struct-spec))

;;;;; CODE GENERATION
(assert-equal
 (translate :c++ '(struct (uttu string)))
 "template
struct Uttu< std::string >;")

(assert-equal
 (translate :c++ '(struct (uttu (g 5))))
 "template < typename _G5 >
struct Uttu {
  _G5 name;
};")

(assert-equal (caster-signature :c++ '(struct (uttu integer)))
              "size_t s_uttu_with_clint (const cl_object o);")


(assert-equal (struct-caster-body '(struct (uttu integer)))
              "return (size_t)new Uttu< int >{
  *(int*)clint(cl_getf(2, o, c_string_to_object(\":NAME\")))
};")

(assert-equal (struct-caster-header '(struct (uttu integer)))
              "size_t clint (const cl_object o);")

(assert-equal (caster-snippet :c++ '(struct (uttu integer)))
"size_t clint (const cl_object o);
size_t s_uttu_with_clint (const cl_object o){
  return (size_t)new Uttu< int >{
    *(int*)clint(cl_getf(2, o, c_string_to_object(\":NAME\")))
  };
}")


(assert-equal (translate :c++ '(struct (monster-struct (g DA) (g OI) (g vec ))))
"template < typename _GDA, typename _GOI, typename _GVEC >
struct MonsterStruct {
  std::string name;
  std::vector< _GDA >* data;
  struct  {
    struct  {
      _GDA ipv4;
      int ipv6;
    } ip;
    _GOI timeout;
  } connection;
  struct  {
    std::vector< _GDA > pphh;
    std::vector< _GDA > pppp;
    std::vector< _GDA > hhhh;
    const std::vector< _GDA > lens;
  } components;
  const struct  {
    _GVEC* date;
  } in;
  std::vector< _GOI > lens;
};")

(assert-equal
 (translate :c++ '(struct (monster-struct integer double-float single-float)))
 "template
struct MonsterStruct< int, double, float >;")

(typep '(pointer (struct monster-struct))
       '(or (pointer struct-identifier) (pointer struct-spec)))

(typep '(const (struct monster-struct))
       '(or (const struct-identifier) (const struct-spec)))

#+nil (assert-equal
 (caster-snippet :c++ '(struct (monster-struct integer double-float single-float)))
"size_t clstr (const cl_object o);
size_t pv_of_clint (const cl_object o);
size_t cs_nil (const cl_object o);
size_t v_of_cldouble (const cl_object o);
size_t s_monster_struct_with_clint_and_cldouble_and_clfloat (const cl_object o){
  return (size_t)new MonsterStruct< int, double, float >{
    *(std::string*)clstr(cl_getf(2, o, c_string_to_object(\":NAME\"))),
    *(std::vector< int >**)pv_of_clint(cl_getf(2, o, c_string_to_object(\":DATA\"))),
    {
      {
        *(int*)clint(cl_getf(2, cl_getf(2, cl_getf(2, o, c_string_to_object(\":CONNECTION\")), c_string_to_object(\":IP\")), c_string_to_object(\":IPV4\"))),
        *(int*)clint(cl_getf(2, cl_getf(2, cl_getf(2, o, c_string_to_object(\":CONNECTION\")), c_string_to_object(\":IP\")), c_string_to_object(\":IPV6\")))
      } /* unnamed */
    ,
      *(double*)cldouble(cl_getf(2, cl_getf(2, o, c_string_to_object(\":CONNECTION\")), c_string_to_object(\":TIMEOUT\")))
    } /* unnamed */
  ,
    {
      *(std::vector< int >*)v_of_clint(cl_getf(2, cl_getf(2, o, c_string_to_object(\":COMPONENTS\")), c_string_to_object(\":PPHH\"))),
      *(std::vector< int >*)v_of_clint(cl_getf(2, cl_getf(2, o, c_string_to_object(\":COMPONENTS\")), c_string_to_object(\":PPPP\"))),
      *(std::vector< int >*)v_of_clint(cl_getf(2, cl_getf(2, o, c_string_to_object(\":COMPONENTS\")), c_string_to_object(\":HHHH\"))),
      *(const std::vector< int >*)v_of_clint(cl_getf(2, cl_getf(2, o, c_string_to_object(\":COMPONENTS\")), c_string_to_object(\":LENS\")))
    } /* unnamed */
  ,
    {
      *(float**)pclfloat(cl_getf(2, cl_getf(2, o, c_string_to_object(\":IN\")), c_string_to_object(\":DATE\")))
    } /* unnamed */
  ,
    *(std::vector< double >*)v_of_cldouble(cl_getf(2, o, c_string_to_object(\":LENS\")))
  };
}")
(check-type '"hello" (choice string ("hello" "world")))
(check-type '5 (choice integer (1 3 5)))
(check-type '5.5 (choice single-float (1.0 3.0 5.5)))
(check-type '5.5d0 (choice double-float (1.0d0 3.0d0 5.5d0)))
(assert (typep '(:name :mode
                 :type (member :binary :text)
                 :default :binary
                 :required t
                 :doc "The file where the tensor is located")
               'step-setting-spec))
(assert (typep '(:name :mode
                 :type (member :binary :text)
                 :doc "todo")
               'step-setting-spec))
;; TODO: checkout the error messages
(assert-not (ignore-errors (typep '(:name mode
                                    :type (member :binary :text)
                                    :doc "todo")
                                  'step-setting-spec)))
(assert-not (ignore-errors (typep '(:name mode
                                    :type (member :binary :text)
                                    :default :no-name
                                    :doc "todo")
                                  'step-setting-spec)))
(assert-not (ignore-errors (typep '(:name mode
                                    :type (member :binary :text)
                                    :default :no-name)
                                  'step-setting-spec)))
(assert (defstep-keywords))

(setf *mode-spec* '(:name :mode
                    :type (member :binary :text)
                    :doc "Mode of the reading"))

(setf *file-spec* '(:name :file
                    :type string
                    :doc "File name"))

(assert (step-setting-typep '(:mode :binary) `(,*mode-spec* ,*file-spec*)))

(defstep (tensor-reader F)
  :in
  (:name :file
   :type string
   :default "input.dat"
   :required t
   :doc "The file where the tensor is located")
  (:name :mode
   :type F
   :default :binary
   :required t
   :doc "The encoding and format that the tensor is written in")
  :out
  (:name :tensor
   :type (vec F)
   :doc "The file where the tensor is located")
  :run
  ("runTensorReader" F))

(let* ((tr-value '(:in (:file "tensor.dat"
                        :mode 456)
                   :out (:tensor #(4 6 8)))))
  (step/check-type tr-value '(tensor-reader integer))
  (struct/check-type tr-value '(struct (tensor-reader integer)))
  (assert (typep tr-value '(struct (tensor-reader integer))))

  ;; make it fail
  (setf (getf (getf tr-value :in) :mode) 456.5)
  (assert-not (typep '(:in (:file "tensor.dat"
                            :mode 456.5)
                       :out (:tensor #(4 6 8)))
                     '(struct (tensor-reader integer)))))

(make-step
 '(tensor-reader integer)
 :in
 :file "tensor.dat"
 :mode 505
 :out
 :tensor #(5 6 8))

(defvar *a* 65)

($ '(tensor-reader integer)
 :in
 :file "tensor.dat"
 :mode 505
 :out
 :tensor #(5 6 8))
;; (mk-step
;;  'Tensor-Reader
;;  :in
;;  :file "amplitudes.dat"
;;  :mode :binary
;;  :out
;;  :tensor "Whatever")
;; 
;; (mk-stepq
;;  Tensor-Reader
;;  :in
;;  :file "amplitudes.dat"
;;  :mode :binary
;;  :out
;;  :tensor "Whatever")

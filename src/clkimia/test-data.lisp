(in-package :kimia)
(defstruct!
    tensor-reader-double
    ((:name string)
     (:lens (vec double-float))))

(defstruct!
    (tensor-reader-g F)
    ((:name string)
     (:lens (vec F))))

(defstruct!
    (davidson-solver A B tensor-field D)
    ((:vectorspace A)
     (:fields (vec B))
     (:lens (vec tensor-field))
     (:mask-tensor (vec tensor-field))
     (:dimension (vec (vec (vec D)) 5))))

(defstruct!
    (Uttu F)
    ((:name F)))

(defstruct!
    with-unnammed
    ((:name string)
     (:lens (struct nil ((:lens integer))))))

(defstruct!
    with-unnammed-and-simple
    ((:name string)
     (:author (struct nil ((:name string))))
     (:lens (struct with-unnamed))))

;; this example is the MONSTER-STRUCT
(defstruct!
    (monster-struct A B C)
    ((:name string)
     (:data (pointer (vec A)))
     (:connection (struct nil ((:ip (struct nil
                                            ((:ipv4 A)
                                             (:ipv6 integer))))
                               (:timeout B))))
     (:components (struct nil
                          ((:pphh (vec A))
                           (:pppp (vec A))
                           (:hhhh (vec A))
                           (:lens (const (vec A))))))
     (:in (const (struct nil ((:date (pointer C))))))
     (:lens (vec B))))

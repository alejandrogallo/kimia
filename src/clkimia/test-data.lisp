(in-package :kimia)
(defgenericstruct
    tensor-reader-double
    ((:name string)
     (:lens (vec double-float))))

(defgenericstruct
    (tensor-reader F)
    ((:name string)
     (:lens (vec F))))

(defgenericstruct
    (davidson-solver A B tensor-field D)
    ((:vectorspace A)
     (:fields (vec B))
     (:lens (vec tensor-field))
     (:mask-tensor (vec tensor-field))
     (:dimension (vec (vec (vec D)) 5))))

(defgenericstruct
    (Uttu F)
    ((:name F)))

(defgenericstruct
    with-unnammed
    ((:name string)
     (:lens (struct nil ((:lens integer))))))

(defgenericstruct
    with-unnammed-and-simple
    ((:name string)
     (:author (struct nil ((:name string))))
     (:lens (struct with-unnamed))))

;; this example is the MONSTER-STRUCT
(defgenericstruct
    (monster-struct A B C)
    ((:name string)
     (:data (pointer (vec (const A))))
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

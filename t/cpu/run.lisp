(defpackage :boem.t.cpu.run
  (:use :cl))
(in-package :boem.t.cpu.run)

(defmethod boem.cpu::mem8-get ((mem list) (addr integer))
  (nth addr mem))

(defmethod boem.cpu::mem8-set ((mem list) (addr integer) (int8 integer))
  (setf (nth addr mem) int8))

(defmethod boem.cpu::mem8-get ((mem hash-table) (addr integer))
  (gethash addr mem))

(defmethod boem.cpu::mem8-set ((mem hash-table)
                                (addr integer) (int8 integer))
  (setf (gethash addr mem) int8))

(defmacro ldr8r8 (&key test)
  `(let ((set (boem.cpu::make-register-set
               :pc 0
               :bc #x0000
               :de #x0012)))
     (boem.cpu::run (boem.cpu::make-ldr8r8 :x :b :y :e) nil set)
     (,test (= (boem.cpu::b-get set) #x12))
     (,test (= (boem.cpu::pc-get set) 1))))

(defmacro ldr8d8 (&key test)
  `(let ((set (boem.cpu::make-register-set
               :pc 0
               :bc #x0000)))
     (boem.cpu::run (boem.cpu::make-ldr8d8 :r :b :d #x12) nil set)
     (,test (= (boem.cpu::b-get set) #x12))
     (,test (= (boem.cpu::pc-get set) 2))))

(defmacro ldr8hl (&key test)
  `(let ((set (boem.cpu::make-register-set
               :pc 0
               :bc #x0000
               :hl #x0000)))
     (boem.cpu::run (boem.cpu::make-ldr8hl :r :b) (list #x12) set)
     (,test (= (boem.cpu::b-get set) #x12))
     (,test (= (boem.cpu::pc-get set) 1))))

(defmacro ldhlr8 (&key test)
  `(let ((mem (list #x00))
         (set (boem.cpu::make-register-set
               :pc 0
               :bc #x1200
               :hl #x0000)))
     (boem.cpu::run (boem.cpu::make-ldhlr8 :r :b) mem set)
     (,test (= (nth 0 mem) #x12))
     (,test (= (boem.cpu::pc-get set) 1))))

(defmacro ldhld8 (&key test)
  `(let ((mem (list #x00))
         (set (boem.cpu::make-register-set
               :pc 0
               :hl #x0000)))
     (boem.cpu::run (boem.cpu::make-ldhld8 :d #x12) mem set)
     (,test (= (nth 0 mem) #x12))
     (,test (= (boem.cpu::pc-get set) 2))))

(defmacro ldabc (&key test)
  `(let ((mem (list #x12))
         (set (boem.cpu::make-register-set
               :pc 0
               :af #x0000
               :bc #x0000)))
     (boem.cpu::run (boem.cpu::make-ldabc) mem set)
     (,test (= (boem.cpu::a-get set) #x12))
     (,test (= (boem.cpu::pc-get set) 1))))

(defmacro ldade (&key test)
  `(let ((mem (list #x12))
         (set (boem.cpu::make-register-set
               :pc 0
               :af #x0000
               :de #x0000)))
     (boem.cpu::run (boem.cpu::make-ldade) mem set)
     (,test (= (boem.cpu::a-get set) #x12))
     (,test (= (boem.cpu::pc-get set) 1))))

(defmacro ldad16 (&key test)
  `(let ((mem (list #x12))
         (set (boem.cpu::make-register-set
               :pc 0
               :af #x0000)))
     (boem.cpu::run (boem.cpu::make-ldad16 :d #x0000) mem set)
     (,test (= (boem.cpu::a-get set) #x12))
     (,test (= (boem.cpu::pc-get set) 3))))

(defmacro ldbca (&key test)
  `(let ((mem (list #x00))
         (set (boem.cpu::make-register-set
               :pc 0
               :bc #x0000
               :af #x1200)))
     (boem.cpu::run (boem.cpu::make-ldbca) mem set)
     (,test (= (nth 0 mem) #x12))
     (,test (= (boem.cpu::pc-get set) 1))))

(defmacro lddea (&key test)
  `(let ((mem (list #x00))
         (set (boem.cpu::make-register-set
               :pc 0
               :de #x0000
               :af #x1200)))
     (boem.cpu::run (boem.cpu::make-lddea) mem set)
     (,test (= (nth 0 mem) #x12))
     (,test (= (boem.cpu::pc-get set) 1))))

(defmacro ldd16a (&key test)
  `(let ((mem (list #x00))
         (set (boem.cpu::make-register-set
               :pc 0
               :af #x1200)))
     (boem.cpu::run (boem.cpu::make-ldd16a :d #x0000) mem set)
     (,test (= (nth 0 mem) #x12))
     (,test (= (boem.cpu::pc-get set) 3))))

(defmacro ldaff00+d8 (&key test)
  `(let ((mem (make-hash-table :test #'equal))
         (set (boem.cpu::make-register-set
               :pc 0
               :af #x0000)))
     (boem.cpu::mem8-set mem #xFF01 #x12)
     (boem.cpu::run (boem.cpu::make-ldaff00+d8 :d #x01) mem set)
     (,test (= (boem.cpu::a-get set) #x12))
     (,test (= (boem.cpu::pc-get set) 2))))

(defmacro ldff00+d8a (&key test)
  `(let ((mem (make-hash-table :test #'equal))
         (set (boem.cpu::make-register-set
               :pc 0
               :af #x1200)))
     (boem.cpu::run (boem.cpu::make-ldff00+d8a :d #x01) mem set)
     (,test (= (boem.cpu::mem8-get mem #xFF01) #x12))
     (,test (= (boem.cpu::pc-get set) 2))))

(defmacro ldaff00+c (&key test)
  `(let ((mem (make-hash-table :test #'equal))
         (set (boem.cpu::make-register-set
               :pc 0
               :bc #x0001
               :af #x0000)))
     (boem.cpu::mem8-set mem #xFF01 #x12)
     (boem.cpu::run (boem.cpu::make-ldaff00+c) mem set)
     (,test (= (boem.cpu::a-get set) #x12))
     (,test (= (boem.cpu::pc-get set) 1))))

(defmacro ldff00+ca (&key test)
  `(let ((mem (make-hash-table :test #'equal))
         (set (boem.cpu::make-register-set
               :pc 0
               :bc #x0001
               :af #x1200)))
     (boem.cpu::run (boem.cpu::make-ldff00+ca) mem set)
     (,test (= (boem.cpu::mem8-get mem #xFF01) #x12))
     (,test (= (boem.cpu::pc-get set) 1))))

(defmacro ldihla (&key test)
  `(let ((mem (list #x00))
         (set (boem.cpu::make-register-set
               :pc 0
               :hl #x0000
               :af #x1200)))
     (boem.cpu::run (boem.cpu::make-ldihla) mem set)
     (,test (= (nth 0 mem) #x12))
     (,test (= (boem.cpu::hl-get set) #x0001))
     (,test (= (boem.cpu::pc-get set) 1))))

(defmacro ldiahl (&key test)
  `(let ((mem (list #x12))
         (set (boem.cpu::make-register-set
               :pc 0
               :hl #x0000
               :af #x0000)))
     (boem.cpu::run (boem.cpu::make-ldiahl) mem set)
     (,test (= (boem.cpu::a-get set) #x12))
     (,test (= (boem.cpu::hl-get set) #x0001))
     (,test (= (boem.cpu::pc-get set) 1))))

(defmacro lddhla (&key test)
  `(let ((mem (list nil #x00))
         (set (boem.cpu::make-register-set
               :pc 0
               :hl #x0001
               :af #x1200)))
     (boem.cpu::run (boem.cpu::make-lddhla) mem set)
     (,test (= (nth 1 mem) #x12))
     (,test (= (boem.cpu::hl-get set) #x0000))
     (,test (= (boem.cpu::pc-get set) 1))))

(defmacro lddahl (&key test)
  `(let ((mem (list nil #x12))
         (set (boem.cpu::make-register-set
               :pc 0
               :hl #x0001
               :af #x0000)))
     (boem.cpu::run (boem.cpu::make-lddahl) mem set)
     (,test (= (boem.cpu::a-get set) #x12))
     (,test (= (boem.cpu::hl-get set) #x0000))
     (,test (= (boem.cpu::pc-get set) 1))))


(boem.t:add-tests :cpu.run
 ldr8r8
 ldr8d8
 ldr8hl
 ldhlr8
 ldhld8
 ldabc
 ldade
 ldad16
 ldbca
 lddea
 ldd16a
 ldaff00+d8
 ldff00+d8a
 ldaff00+c
 ldff00+ca
 ldihla
 ldiahl
 lddhla
 lddahl)

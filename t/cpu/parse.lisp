(defpackage :boem.t.cpu.parse
  (:use :cl))
(in-package :boem.t.cpu.parse)

(defmacro ldr8r8 (&key test)
  `(let ((cmd (boem.cpu::parse (list #b01001011) 0)))
     (,test (eq (type-of cmd) 'boem.cpu::ldr8r8))
     (,test (eql (boem.cpu::ldr8r8-x cmd) boem.cpu::+c+))
     (,test (eql (boem.cpu::ldr8r8-y cmd) boem.cpu::+e+))))

(defmacro ldr8d8 (&key test)
  `(let ((cmd (boem.cpu::parse (list #b00001110 #x12) 0)))
     (,test (eq (type-of cmd) 'boem.cpu::ldr8d8))
     (,test (eql (boem.cpu::ldr8d8-r cmd) boem.cpu::+c+))
     (,test (= (boem.cpu::ldr8d8-d cmd) #x12))))

(defmacro ldr8hl (&key test)
  `(let ((cmd (boem.cpu::parse (list #b01001110) 0)))
     (,test (eq (type-of cmd) 'boem.cpu::ldr8hl))
     (,test (eql (boem.cpu::ldr8hl-r cmd) boem.cpu::+c+))))

(defmacro ldhlr8 (&key test)
  `(let ((cmd (boem.cpu::parse (list #b01110001) 0)))
     (,test (eq (type-of cmd) 'boem.cpu::ldhlr8))
     (,test (eql (boem.cpu::ldhlr8-r cmd) boem.cpu::+c+))))

(defmacro ldhld8 (&key test)
  `(let ((cmd (boem.cpu::parse (list #b00110110 #x12) 0)))
     (,test (eq (type-of cmd) 'boem.cpu::ldhld8))
     (,test (eql (boem.cpu::ldhld8-d cmd) #x12))))

(defmacro ldabc (&key test)
  `(let ((cmd (boem.cpu::parse (list #b00001010) 0)))
     (,test (eq (type-of cmd) 'boem.cpu::ldabc))))

(defmacro ldade (&key test)
  `(let ((cmd (boem.cpu::parse (list #b00011010) 0)))
     (,test (eq (type-of cmd) 'boem.cpu::ldade))))

(defmacro lda16 (&key test)
  `(let ((cmd (boem.cpu::parse (list #b11111010 #x12 #x34) 0)))
     (,test (eq (type-of cmd) 'boem.cpu::ldad16))
     (,test (eql (boem.cpu::ldad16-d cmd) #x3412))))

(boem.t:add-tests
 :cpu.parse
 ldr8r8
 ldr8d8
 ldr8hl
 ldhlr8
 ldhld8
 ldabc
 ldade
 lda16)

(defpackage :boem.t
  (:use :cl)
  (:export :add-tests))
(in-package :boem.t)

(fiveam:def-suite* :boem.t)

(defmacro add-tests (name &rest syms)
  (let ((full-name (intern (concatenate 'string
                            "BOEM.T." (string-upcase name))
                           :keyword)))
    `(progn
       (fiveam:def-suite ,full-name :in :boem.t)
       ,@(mapcar (lambda (sym)
                   `(fiveam:def-test ,sym (:suite ,full-name)
                      (,sym :test fiveam:is)))
                 syms))))

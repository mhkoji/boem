(defpackage :boem.t.rendering
  (:use :cl))
(in-package :boem.t.rendering)

(defmethod boem.rendering::mem8-get ((mem list) (addr integer))
  (nth addr mem))

(defmacro tile-pixels (&key test)
  `(let ((tile (boem.rendering::make-tile
                :memory '(#x7C #x7C
                          #x00 #xC6
                          #xC6 #x00
                          #x00 #xFE
                          #xC6 #xC6
                          #x00 #xC6
                          #xC6 #x00
                          #x00 #x00)
                :addr 0)))
     (,test (equalp (boem.rendering::tile-pixels tile)
                    #2A((0 3 3 3 3 3 0 0)
                        (2 2 0 0 0 2 2 0)
                        (1 1 0 0 0 1 1 0)
                        (2 2 2 2 2 2 2 0)
                        (3 3 0 0 0 3 3 0)
                        (2 2 0 0 0 2 2 0)
                        (1 1 0 0 0 1 1 0)
                        (0 0 0 0 0 0 0 0))))))

(boem.t:add-tests :rendering
 tile-pixels)

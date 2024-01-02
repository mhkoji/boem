(asdf:defsystem :boem-sdl
  :serial t
  :pathname "src/"
  :components
  ((:file "sdl"))
  :depends-on (:boem
               :lispbuilder-sdl))

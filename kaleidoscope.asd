;;;; kaleidoscope.asd

(asdf:defsystem #:kaleidoscope
  :description "kaleidoscope images generator"
  :author "José Miguel Ángel Ronquillo Rivera <jose@rufina.link>"
  :license "GPL Ver. 3"
  :version "0.0.1"
  :serial t
  :depends-on (#:bordeaux-threads
               #:alexandria
               #:lparallel
               #:3d-vectors
               #:mcclim
               #:mcclim-raster-image)
  :components ((:file "package")
               (:file "objects")
               (:file "kaleidoscope")))

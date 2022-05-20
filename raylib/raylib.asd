(asdf:defsystem #:raylib
  :version "0.1.0"
  :defsystem-depends-on (:cffi-grovel)
  :depends-on (:cffi-libffi)
  :serial t
  :components ((:file "package")
               (:file "library")
               (:cffi-grovel-file "types")
               (:file "constants")
               (:file "functions")))

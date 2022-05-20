(in-package :raylib)

(define-foreign-library libraylib
  (:darwin "libraylib.dylib")
  (:unix "libraylib.so")
  (:windows "raylib.dll")
  (t (:default "libraylib")))

(use-foreign-library libraylib)

(ql:quickload "slynk")
(slynk:create-server :port 4005 :dont-close t)

(require :asdf)

(push "raylib/" asdf:*central-registry*)

(ql:quickload :raylib)
(ql:quickload :nibbles)

(load "wav.lisp")
(load "sound.lisp")
(load "sprite.lisp")
(load "font.lisp")
(load "main.lisp")

(in-package :raylib)

(main)

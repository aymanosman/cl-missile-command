(require :asdf)

(push "raylib/" asdf:*central-registry*)

(ql:quickload :raylib)
(ql:quickload :nibbles)

(load "wav.lisp")
(load "sound.lisp")
(load "sprite.lisp")
(load "font.lisp")
(load "main.lisp")

(sb-ext:save-lisp-and-die "missile-command" :executable t :toplevel 'raylib::main)

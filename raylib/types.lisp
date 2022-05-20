(in-package :raylib)

(include "raylib.h")

(cstruct audio-stream "AudioStream"
         (buffer "buffer" :type :pointer)
         (sample-rate "sampleRate" :type :unsigned-int)
         (sample-size "sampleSize" :type :unsigned-int)
         (channels "channels" :type :unsigned-int))

(cstruct wave "Wave"
         (frame-count "frameCount" :type :unsigned-int)
         (sample-rate "sampleRate" :type :unsigned-int)
         (sample-size "sampleSize" :type :unsigned-int)
         (channels "channels" :type :unsigned-int)
         (data "data" :type :pointer))

(cstruct sound "Sound"
         (stream "stream" :type (:struct audio-stream))
         (frame-count "frameCount" :type :unsigned-int))

(cstruct texture "Texture"
         (id "id" :type :unsigned-int)
         (width "width" :type :int)
         (height "height" :type :int)
         (mipmaps "mipmaps" :type :int)
         (format "format" :type :int))

(cstruct image "Image"
         (data "data" :type :pointer)
         (width "width" :type :int)
         (height "height" :type :int)
         (mipmaps "mipmaps" :type :int)
         (format "format" :type :int))

(cstruct render-texture "RenderTexture"
         (id "id" :type :unsigned-int)
         (texture "texture" :type (:struct texture))
         (depth "depth" :type (:struct texture)))

(cstruct color "Color"
         (r "r" :type :unsigned-char)
         (g "g" :type :unsigned-char)
         (b "b" :type :unsigned-char)
         (a "a" :type :unsigned-char))

(cenum mouse-button
       ((:left "MOUSE_BUTTON_LEFT"))
       ((:right "MOUSE_BUTTON_RIGHT"))
       ((:middle "MOUSE_BUTTON_MIDDLE")))

(cenum keyboard-key
       ((:p "KEY_P"))
       ((:r "KEY_R"))
       ((:x "KEY_X"))
       ((:z "KEY_Z"))
       ((:space "KEY_SPACE"))
       ((:tab "KEY_TAB"))
       ((:enter "KEY_ENTER")))

(cstruct vector2 "Vector2"
         (x "x" :type :float)
         (y "y" :type :float))

(cstruct rectangle "Rectangle"
         (x "x" :type :float)
         (y "y" :type :float)
         (width "width" :type :float)
         (height "height" :type :float))

(cstruct shader "Shader"
         (id "id" :type :unsigned-int)
         (locs "locs" :type (:pointer :int)))

(cstruct camera2d "Camera2D"
         (offset "offset" :type (:struct vector2))
         (target "target" :type (:struct vector2))
         (rotation "rotation" :type :float)
         (zoom "zoom" :type :float))

(cenum shader-uniform
       ((:float "SHADER_UNIFORM_FLOAT"))
       ((:vec2 "SHADER_UNIFORM_VEC2"))
       ((:vec3 "SHADER_UNIFORM_VEC3"))
       ((:vec4 "SHADER_UNIFORM_VEC4"))
       ((:int "SHADER_UNIFORM_INT"))
       ((:ivec2 "SHADER_UNIFORM_IVEC2"))
       ((:ivec3 "SHADER_UNIFORM_IVEC3"))
       ((:ivec4 "SHADER_UNIFORM_IVEC4"))
       ((:sampler2d "SHADER_UNIFORM_SAMPLER2D")))

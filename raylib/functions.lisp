(in-package :raylib)

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-int:set-floating-point-modes :traps nil))

(defcfun ("InitWindow" init-window) :void
  (width :int)
  (height :int)
  (title :string))

(defcfun ("CloseWindow" close-window) :void)

(defcfun ("WindowShouldClose" window-should-close) :bool)

(defcfun ("InitAudioDevice" init-audio-device) :void)

(defcfun ("CloseAudioDevice" close-audio-device) :void)

(defcfun ("SetTargetFPS" set-target-fps) :void
  (fps :int))

(defcfun ("LoadWaveFromMemory" load-wave-from-memory) (:struct wave)
  (file-type :string)
  (file-data (:pointer :unsigned-char))
  (data-size :int))

(defun make-wave-from-memory (file-type file-data)
  (with-foreign-object (file-data-ptr :uint8 (length file-data))
    (loop :for i :from 0 :below (length file-data)
          :do (setf (mem-aref file-data-ptr :uint8 i) (aref file-data i)))
    (load-wave-from-memory file-type file-data-ptr (length file-data))))

(defcfun ("LoadSoundFromWave" load-sound-from-wave) (:struct sound)
  (wave (:struct wave)))

(defcfun ("LoadSound" load-sound) (:struct sound)
  (filename :string))

(defcfun ("PlaySound" play-sound) :void
  (sound (:struct sound)))

(defcfun ("LoadTexture" load-texture) (:struct texture)
  (file-name :string))

(defun texture-width (object)
  (getf object 'width))

(defun texture-height (object)
  (getf object 'height))

(defcfun ("BeginDrawing" begin-drawing) :void)

(defun make-color (&key r g b a)
  (list 'r r 'g g 'b b 'a a))

(defcfun ("ClearBackground" clear-background) :void
  (color (:struct color)))

(defcfun ("EndDrawing" end-drawing) :void)

(defcfun ("IsMouseButtonPressed" is-mouse-button-pressed) :boolean
  (button mouse-button))

(defcfun ("IsKeyPressed" is-key-pressed) :boolean
  (key keyboard-key))

(defcfun ("DrawFPS" draw-fps) :void
  (pos-x :int)
  (pos-y :int))

(defun make-vector2 (&key x y)
  (list 'x (coerce x 'float)
        'y (coerce y 'float)))

(defun vector2-x (object)
  (getf object 'x))

(defun vector2-y (object)
  (getf object 'y))

(defcfun ("GetMousePosition" get-mouse-position) (:struct vector2))

(defcfun ("GetMouseX" get-mouse-x) :int)

(defcfun ("GetMouseY" get-mouse-y) :int)

(defun make-rectangle (&key x y width height)
  (list 'x (coerce x 'float)
        'y (coerce y 'float)
        'width (coerce width 'float)
        'height (coerce height 'float)))

(defun rectangle-x (object)
  (getf object 'x))

(defun (setf rectangle-x) (new-value object)
  (setf (getf object 'x) new-value))

(defun rectangle-y (object)
  (getf object 'y))

(defun (setf rectangle-y) (new-value object)
  (setf (getf object 'y) new-value))

(defun rectangle-width (object)
  (getf object 'width))

(defun rectangle-height (object)
  (getf object 'height))

(defcfun ("DrawTexture" draw-texture) :void
  (texture (:struct texture))
  (pos-x :int)
  (pos-y :int)
  (tint (:struct color)))

(defcfun ("DrawTextureRec" draw-texture-rec) :void
  (texture (:struct texture))
  (source (:struct rectangle))
  (position (:struct vector2))
  (tint (:struct color)))

(defcfun ("LoadShader" load-shader) (:struct shader)
  (vs-filename :string)
  (fs-filename :string))

(defcfun ("LoadShaderFromMemory" load-shader-from-memory) (:struct shader)
  (vs-code :string)
  (fs-code :string))

(defcfun ("BeginMode2D" begin-mode-2d) :void
  (camera (:struct camera2d)))

(defcfun ("EndMode2D" end-mode-2d) :void)

(defcfun ("BeginShaderMode" begin-shader-mode) :void
  (shader (:struct shader)))

(defcfun ("EndShaderMode" end-shader-mode) :void)

(defun make-camera2d (&key offset target rotation zoom)
  (list 'offset offset
        'target target
        'rotation (coerce rotation 'float)
        'zoom (coerce zoom 'float)))

(defcfun ("GetFrameTime" get-frame-time) :float)

(defcfun ("SetShaderValue" set-shader-value) :void
  (shader (:struct shader))
  (loc-index :int)
  (value :pointer)
  (uniform-type shader-uniform))

(defun set-shader-float (shader loc value)
  (with-foreign-object (float :float)
    (setf (mem-ref float :float) (coerce value 'float))
    (set-shader-value shader loc float :float)))

(defun set-shader-vec2 (shader loc x y)
  (with-foreign-object (array :float 2)
    (setf (mem-aref array :float 0) (coerce x 'float))
    (setf (mem-aref array :float 1) (coerce y 'float))
    (set-shader-value shader loc array :vec2)))

(defcfun ("GetShaderLocation" get-shader-location) :int
  (shader (:struct shader))
  (uniform-name :string))

(defcfun ("GetScreenWidth" get-screen-width) :int)

(defcfun ("GetScreenHeight" get-screen-height) :int)

(defcfun ("DrawLine" draw-line) :void
  (start-pos-x :int)
  (start-pos-y :int)
  (end-pos-x :int)
  (end-pos-y :int)
  (color (:struct color)))

(defcfun ("DrawLineV" draw-line-v) :void
  (start-pos (:struct vector2))
  (end-pos (:struct vector2))
  (color (:struct color)))

(defcfun ("LoadRenderTexture" load-render-texture) (:struct render-texture)
  (width :int)
  (height :int))

(defcfun ("UnloadRenderTexture" unload-render-texture) :void
  (target (:struct render-texture)))

(defun render-texture-texture (object)
  (getf object 'texture))

(defcfun ("DrawTexturePro" draw-texture-pro) :void
  (texture (:struct texture))
  (source (:struct rectangle))
  (dest (:struct rectangle))
  (origin (:struct vector2))
  (rotation :float)
  (color (:struct color)))

(defcfun ("BeginTextureMode" begin-texture-mode) :void
  (target (:struct render-texture)))

(defcfun ("EndTextureMode" end-texture-mode) :void)

(defcfun ("DrawText" draw-text) :void
  (text :string)
  (pos-x :int)
  (pos-y :int)
  (font-size :int)
  (color (:struct color)))

(defcfun ("DrawRectangle" draw-rectangle) :void
  (pos-x :int)
  (pos-y :int)
  (width :int)
  (height :int)
  (color (:struct color)))

(defcfun ("DrawRectangleRec" draw-rectangle-rec) :void
  (rec (:struct rectangle))
  (color (:struct color)))

(defcfun ("DrawCircle" draw-circle) :void
  (center-x :int)
  (center-y :int)
  (radius :float)
  (color (:struct color)))

(defcfun ("LoadTextureFromImage" load-texture-from-image) (:struct texture)
  (image (:struct image)))

(defcfun ("GenImageColor" gen-image-color) (:struct image)
  (width :int)
  (height :int)
  (color (:struct color)))

(defun alloc-image (image)
  (let ((pointer (foreign-alloc '(:struct image))))
    (with-foreign-slots ((width height mipmaps format) pointer (:struct image))
      (setf (foreign-slot-value pointer '(:struct image) 'data) (getf image 'data))
      (setf width (getf image 'width))
      (setf height (getf image 'height))
      (setf mipmaps (getf image 'mipmaps))
      (setf format (getf image 'format)))
    pointer))

(defcfun ("ImageDrawPixel" image-draw-pixel) :void
  (dst (:pointer (:struct image)))
  (pos-x :int)
  (pos-y :int)
  (color (:struct color)))

(defcfun ("ImageFlipVertical" image-flip-vertical) :void
  (image (:pointer (:struct image))))

(defcfun ("CheckCollisionRecs" check-collision-recs) :bool
  (rec1 (:struct rectangle))
  (rec2 (:struct rectangle)))

(defcfun ("CheckCollisionCircleRec" check-collision-circle-rec) :bool
  (center (:struct vector2))
  (radius :float)
  (rec (:struct rectangle)))

(defcfun ("GetCollisionRec" get-collision-rec) (:struct rectangle)
  (rec1 (:struct rectangle))
  (rec2 (:struct rectangle)))

(defcfun ("MeasureText" measure-text) :int
  (text :string)
  (font-size :int))

(defcfun ("ShowCursor" show-cursor) :void)

(defcfun ("HideCursor" hide-cursor) :void)

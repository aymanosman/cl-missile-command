(in-package :raylib)

(defvar frame 0)
(defvar paused nil)
(defvar shader)
(defvar target)
(defvar scale)
(defvar camera)
(defvar screen-width 600)
(defvar screen-height 600)
(defvar game-screen-width 160)
(defvar game-screen-height 128)
(defvar screen-x-offset)
(defvar screen-y-offset)
(defvar city-texture)
(defvar destroyed-texture)
(defvar silo-texture)
(defvar reticle-texture)
(defvar target-texture)
(defvar game-state :menu)
(defvar launch-sound)
(defvar boom-sound)

;; HELPERS

(defun get-game-mouse-x ()
  (/ (- (get-mouse-x) screen-x-offset)
     scale))

(defun get-game-mouse-y ()
  (/ (- (get-mouse-y) screen-y-offset)
     scale))

(defun dist-sq (x y)
  (+ (* x x) (* y y)))

(defun unit-vector (x1 y1 x2 y2)
  (let* ((dx (- x2 x1))
         (dy (- y2 y1))
         (mag (sqrt (dist-sq dx dy))))
    (values (/ dx mag) (/ dy mag))))

(defun btn-any ()
  ;; TODO Why do :enter and :tab always return T?
  (or #+nil (is-key-pressed :enter)
      #+nil (is-key-pressed :tab)
      (is-key-pressed :space)
      (is-key-pressed :z)
      (is-key-pressed :x)
      (is-mouse-button-pressed :left)))

;;; MODEL

(defvar level-time)
(defvar enemy-missiles)
(defvar player-missiles)
(defvar enemy-explosions)
(defvar player-explosions)
(defvar silos)
(defvar cities)
(defvar destroyed)
(defvar score)
(defvar wave)
(defvar multiplier)
(defvar missile-count)
(defvar enemy-missile-speed)
(defvar player-missile-speed)

(defstruct missile x1 y1 x2 y2 (launch-time 0) (warheads nil))
(defstruct explosion x y r dr)

(defun missile-pos (m &key speed)
  (with-slots (x1 y1 x2 y2 launch-time) m
    (multiple-value-bind (dx dy) (unit-vector x1 y1 x2 y2)
      (values (+ x1 (* dx speed (- level-time launch-time)))
              (+ y1 (* dy speed (- level-time launch-time)))))))


(defun check-collision-missile-explosion (m e)
  (with-slots (x y r) e
    (check-collision-circle-rec (make-vector2 :x x :y y)
                                r
                                (multiple-value-bind (x y) (missile-pos m :speed enemy-missile-speed)
                                  (make-rectangle :x x
                                                  :y y
                                                  :width 2
                                                  :height 2)))))

(defun level-bonus ()
  "Returns (values MISSILE-BONUS CITY-BONUS)"
  (values (* missile-count 5)
          (* (length cities) 100)))

;;; UPDATE

(defun setup ()
  (setf level-time 0
        wave 0
        score 0
        multiplier 0
        missile-count 0
        enemy-missile-speed (+ 10 wave)
        player-missile-speed 200)

  (setf enemy-missiles nil)
  (setf player-missiles nil)
  (setf player-explosions nil)
  (setf enemy-explosions nil)

  (setf silos (list 5 79 153))
  (setf cities (list 20 40 60 100 120 140))
  (setf destroyed nil))

(defun random-ref (choices)
  (nth (random (+ 1 (length choices))) choices))

(defun make-enemy-missile (&key x1 y1 mirv (launch-time 0))
  (let ((x1 (or x1 (random game-screen-width)))
        (y1 (or y1 (- (random (/ game-screen-height 2)))))
        (x2 (or (random-ref (append cities silos))
                (random game-screen-width)))
        (y2  (- game-screen-height 4)))
    (make-missile :x1 x1 :y1 y1
                  :x2 x2 :y2 y2
                  :launch-time launch-time
                  :warheads mirv)))

(defun launch-enemy-missile (&rest args)
  (push (apply #'make-enemy-missile args) enemy-missiles))

(defun closest-silo (target-x)
  (let ((min-x (first silos)))
    (dolist (x (rest silos))
      (when (< (abs (- x target-x))
               (abs (- min-x target-x)))
        (setf min-x x)))
    min-x))

(defun launch-player-missile ()
  (when (and (not (null silos))
             (> missile-count 0))
    (let* ((x2 (get-game-mouse-x))
           (y2 (get-game-mouse-y))
           (x1 (closest-silo x2))
           (y1 (- game-screen-height 10)))
      (push (make-missile :x1 x1 :y1 y1
                          :x2 x2 :y2 y2
                          :launch-time level-time)
            player-missiles)
      (play-sound launch-sound)
      (decf missile-count))))

(defun update-explosion (e)
  (with-slots (x y r dr) e
    (setf r (+ r (* (get-frame-time) dr)))
    (when (>= r 10.0)
      (setf dr -10.0))))

(defun update-enemy-explosions ()
  (dolist (e enemy-explosions)
    (update-explosion e))

  (setf enemy-explosions (remove-if (lambda (e) (<= (explosion-r e) 0.0)) enemy-explosions)))

(defun update-player-explosions ()
  (dolist (e player-explosions)
    (update-explosion e))

  (setf player-explosions (remove-if (lambda (e) (<= (explosion-r e) 0.0)) player-explosions)))

(defun destroy-city (target-x)
  (setf cities (remove-if (lambda (x)
                            (when (= target-x x)
                              (push x destroyed)))
                          cities)))

(defun destroy-silo (target-x)
  (setf silos (remove-if (lambda (x) (= target-x x)) silos)))

(defun update-enemy-missiles ()
  (let (new-missiles)
    (setf enemy-missiles
          (remove-if (lambda (missile)
                       (with-slots (x1 y1 x2 y2 warheads) missile
                         (multiple-value-bind (x y) (missile-pos missile :speed enemy-missile-speed)
                           (cond
                             ;; collides with player explosion
                             ((some (lambda (e)
                                      (check-collision-missile-explosion missile e))
                                    player-explosions)
                              (push (make-explosion :x x
                                                    :y y
                                                    :r 1.0
                                                    :dr 10.0)
                                    player-explosions)
                              (play-sound boom-sound)
                              (incf score (if warheads 50 25))
                              t)
                             ;; mirv split
                             ((and warheads
                                   (>= (/ (sqrt (dist-sq (- x1 x) (- y1 y)))
                                          (sqrt (dist-sq (- x1 x2) (- y1 y2))))
                                       0.5))
                              (dotimes (n warheads)
                                (push (make-enemy-missile :x1 x :y1 y
                                                          :launch-time level-time)
                                      new-missiles))
                              ;; turn into normal missile
                              (setf (missile-warheads missile) nil)
                              ;; the original mirv survives as a normal missile
                              nil)
                             ;; collides with ground
                             ((>= y (- game-screen-height 8))
                              (destroy-city x2)
                              (destroy-silo x2)
                              (push (make-explosion :x x
                                                    :y y
                                                    :r 1.0
                                                    :dr 10.0)
                                    enemy-explosions)
                              (play-sound boom-sound)
                              t)
                             (t
                              nil)))))
                     enemy-missiles))
    (setf enemy-missiles (nconc new-missiles enemy-missiles))))

(defun explode-player-missile (x y)
  (push (make-explosion :x x
                        :y y
                        :r 1.0
                        :dr 10.0)
        player-explosions)
  (play-sound boom-sound))

(defun update-player-missiles ()
  (when (is-mouse-button-pressed :left)
    (launch-player-missile))

  (setf player-missiles
        (remove-if (lambda (missile)
                     (with-slots (x1 y1 x2 y2 launch-time) missile
                       (multiple-value-bind (px py) (missile-pos missile :speed player-missile-speed)
                         (when (>= (dist-sq (- x1 px) (- y1 py))
                                   (dist-sq (- x1 x2) (- y1 y2)))
                           (explode-player-missile px py)
                           t))))
                   player-missiles)))

(defun next-wave ()
  (incf wave)
  (setf level-time 0
        enemy-missile-speed (+ 10 wave))

  ;; increment the multiplier every other wave
  (when (= (logand wave 1) 1)
    (incf multiplier))

  (dotimes (n (+ 5 wave))
    (let ((mirv (if (zerop (mod n 5)) (+ (random 4) 1) nil)))
      (launch-enemy-missile :mirv mirv)))

  (incf missile-count 10)

  (setf game-state :level))

;;; VIEW

(defvar bg (make-color :r #x10 :g #x10 :b #x18 :a #xff))

(defvar +palette+
  (map 'vector
       (lambda (color)
         (make-color :r (first color)
                     :g (second color)
                     :b (third color)
                     :a (fourth color)))
       '((#x00 #x00 #x00 #xff)
         (#x1d #x2b #x53 #xff)
         (#x7e #x25 #x53 #xff)
         (#x00 #x87 #x51 #xff)
         (#xab #x52 #x36 #xff)
         (#x5f #x57 #x4f #xff)
         (#xc2 #xc3 #xc7 #xff)
         (#xff #xf1 #xe8 #xff)
         (#xff #x00 #x4d #xff)
         (#xff #xa3 #x00 #xff)
         (#xff #xec #x27 #xff)
         (#x00 #xe4 #x36 #xff)
         (#x29 #xad #xff #xff)
         (#x83 #x76 #x9c #xff)
         (#xff #x77 #xab #xff)
         (#xff #xcc #xaa #xff))))

(defun color (n)
  (aref +palette+ n))

(defun draw-frame ()
  (begin-drawing)
  (begin-mode-2d camera)
  (clear-background bg)
  (begin-shader-mode shader)
  (draw-texture-rec (render-texture-texture target)
                    (make-rectangle :x 0 :y 0
                                    :width game-screen-width
                                    :height (- game-screen-height))
                    (make-vector2 :x 0 :y 0)
                    +white+)
  (end-shader-mode)
  (end-mode-2d)
  (end-drawing))

(defun draw (x y color texture)
  (draw-texture-rec texture
                    (make-rectangle :x 0 :y 0
                                    :width (getf texture 'width)
                                    :height (getf texture 'height))
                    (make-vector2 :x x :y y)
                    color))

(defun draw-score ()
  (draw 2 9 (color 9) silo-texture)
  (text 2 2 (color 7) (format nil "Score: ~a" score))
  (text 8 9 (color 7) (format nil "x~a" missile-count)))

(defun menu ()
  (begin-texture-mode target)
  (clear-background +black+)
  (text 60 40 (color 7) (format nil "Ready Wave: ~a" (+ wave 1)))
  (text 60 47 (color 7) "Press Any Button")

  (when (> wave 0)
    (multiple-value-bind (missile-bonus city-bonus) (level-bonus)
      (text 70 61 (color 7) (format nil "x~a +~a" missile-count (* missile-bonus multiplier)))
      (text 70 68 (color 7) (format nil "x~a +~a" (length cities) (* city-bonus multiplier)))

      (draw 61 61 (color 9) silo-texture)
      (draw 64 61 (color 9) silo-texture)

      (draw 60 68 (color 11) city-texture))

    (draw-score))

  (end-texture-mode)

  (when (btn-any)
    (next-wave))

  (draw-frame))

(defun draw-missile (x1 y1 x y &key (color (color 8)))
  ;; trail
  (draw-line-v (make-vector2 :x x1 :y y1)
               (make-vector2 :x x :y y)
               color)
  ;; head
  (draw-rectangle-rec (make-rectangle :x (- x 1)
                                      :y (- y 1)
                                      :width 2
                                      :height 2)
                      (color 7)))

(defun draw-enemy-missile (missile)
  (with-slots (x1 y1) missile
    (multiple-value-bind (x y) (missile-pos missile :speed enemy-missile-speed)
      (draw-missile x1 y1 x y :color (color 8)))))

(defun random-color ()
  (color (+ 8 (logand frame #x7))))

(defun draw-player-missile (missile)
  (with-slots (x1 y1 x2 y2) missile
    (multiple-value-bind (x y) (missile-pos missile :speed player-missile-speed)
      (draw-missile x1 y1 x y :color (color 10))
      (draw (- x2 2) (- y2 2) (random-color) target-texture))))

(defun draw-explosion (e)
  (with-slots (x y r) e
    (draw-circle (round x) (round y) (coerce r 'float) (random-color))))

(defun draw-ground ()
  (draw-rectangle 0 124 game-screen-width 4 (color 2)))

(defun draw-cities ()
  (dolist (city cities)
    (draw (- city 4) 120 (color 12) city-texture)))

(defun draw-destroyed ()
  (dolist (x destroyed)
    (draw (- x 4) 120 (color 4) destroyed-texture)))

(defun draw-enemy-missiles ()
  (dolist (m enemy-missiles)
    (draw-enemy-missile m)))

(defun draw-silos ()
  (dolist (x silos)
    (flet ((draw (x y)
             (draw x y (color 9) silo-texture)))
      (draw (- x 1) 120)
      (draw (- x 4) 122)
      (draw (+ x 2) 122))))

(defun draw-reticle ()
  (let ((x (- (get-game-mouse-x) 2))
        (y (- (get-game-mouse-y) 2)))
    (draw x y (color 7) reticle-texture)))

;; MAIN

(defun level ()
  (when (and (null enemy-missiles)
             (null enemy-explosions)
             (null player-missiles)
             (null player-explosions))
    (if (null cities)
        (setf game-state :game-over)
        (progn
          (setf game-state :menu)
          (incf score (apply #'+ (multiple-value-list (level-bonus))))
          (return-from level))))

  ;; (when (is-key-pressed :r)
  ;;   (setf game-state :menu)
  ;;   (setup)
  ;;   (return-from level))

  ;; (when (is-key-pressed :p)
  ;;   (setf game-state :paused)
  ;;   (return-from level))

  (update-enemy-explosions)
  (update-player-explosions)
  (update-enemy-missiles)
  (update-player-missiles)

  (begin-texture-mode target)
  (clear-background +black+)
  (draw-ground)
  (draw-cities)
  (draw-silos)
  (draw-destroyed)
  (draw-enemy-missiles)
  (dolist (m player-missiles)
    (draw-player-missile m))
  (dolist (e enemy-explosions)
    (draw-explosion e))
  (dolist (e player-explosions)
    (draw-explosion e))
  (draw-reticle)
  (draw-score)
  (end-texture-mode)

  (draw-frame))

(defun game-over ()
  (when (btn-any)
    (setup)
    (setf game-state :menu))

  (begin-texture-mode target)
  (clear-background +black+)
  (text 40 40 (color 7) (format nil "World lost on wave ~a" wave))
  (text 40 47 (color 7) (format nil "Final score: ~a" score))
  (end-texture-mode)

  (draw-frame))

(defun paused ()
  (when (btn-any)
    (setf game-state :level))
  (draw-frame))

(defun game-loop ()
  (if (window-should-close) (return-from game-loop))

  (ecase game-state
    (:menu
     (menu))

    (:level
     (incf level-time (get-frame-time))
     (incf frame)
     (level))

    (:game-over
     (game-over))

    (:paused
     (paused)))

  (game-loop))

(defvar city-sprite #(#*00100000
                      #*01101000
                      #*11101101
                      #*11111111
                      #*11111111))

(defvar destroyed-sprite #(#*00000000
                           #*00000000
                           #*00000000
                           #*10010001
                           #*11010011))

(defvar silo-sprite #(#*01000000
                      #*01000000
                      #*01000000
                      #*11100000
                      #*10100000))

(defvar reticle-sprite #(#*00100000
                         #*00100000
                         #*11011000
                         #*00100000
                         #*00100000))

(defvar target-sprite #(#*10001000
                        #*01010000
                        #*00100000
                        #*01010000
                        #*10001000))

(defun make-camera (scale)
  (flet ((half (n) (/ n 2)))
    (setf screen-x-offset (half (- screen-width (* scale game-screen-width)))
          screen-y-offset (half (- screen-height (* scale game-screen-height)))))
  (make-camera2d :offset (make-vector2 :x screen-x-offset
                                       :y screen-y-offset)
                 :target (make-vector2 :x 0 :y 0)
                 :rotation 0.0
                 :zoom scale))

(defvar scanline-fragment-shader
  "#version 330
precision mediump float;

in vec2 fragTexCoord;
in vec4 fragColor;

out vec4 finalColor;

uniform sampler2D texture0;
uniform vec4 colDiffuse;

void main() {
    vec3 pixel = texture(texture0, fragTexCoord).rgb;

    // every 3rd pixel should be a scanline
    float fmin = 0.50;
    float fmod = mod(gl_FragCoord.y, 3.0);
    float fstep = fmin + (1.0 - fmin) * fmod;

    // alpha the color by the scanline
    finalColor = vec4(pixel, fstep);
}")

(defun main ()
  "Make sure you don't call me inside Emacs!"
  (init-window screen-width screen-height "Defender")
  (init-audio-device)
  (unwind-protect
       (progn
         (set-target-fps 30)
         (hide-cursor)
         (setf scale (max (floor (min (/ screen-width game-screen-width)
                                      (/ screen-height game-screen-height)))
                          1.0))
         (setf camera (make-camera scale))

         (setf font (make-font-from-sprites basic-font :advance 4))

         (flet ((make (sprite)
                  (make-sprite-texture sprite :width 8 :height 5 :color +white+)))
           (setf city-texture (make city-sprite))
           (setf destroyed-texture (make destroyed-sprite))
           (setf silo-texture (make silo-sprite))
           (setf reticle-texture (make reticle-sprite))
           (setf target-texture (make target-sprite)))

         (setf launch-sound (make-sound (sweep 600 400) 0.75 #'sawtooth-wave (fade-out-envelope)))
         (setf boom-sound (make-sound (constantly 50) 1.25 #'noise-wave (fade-out-envelope)))

         (setf game-state :menu)
         (setup)
         (setf target (load-render-texture game-screen-width game-screen-height))
         (setf shader (load-shader-from-memory (null-pointer) scanline-fragment-shader))
         (game-loop))
    (when (boundp 'target)
      (unload-render-texture target))
    (close-audio-device)
    (show-cursor)
    (close-window)))

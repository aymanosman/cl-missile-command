(in-package :raylib)

(defun sine-wave (x)
  (sin x))

(defun square-wave (x)
  (signum (sin x)))

(defun sawtooth-wave (x)
  (let ((y (/ x (* 2 pi))))
    (* 2 (- y (floor (+ y 0.5))))))

(defun noise-wave (x)
  (* (random 1.0) (signum (sin x))))

(defun sweep (start end)
  (lambda (x)
    (- start (* x (- start end)))) )

(defun fade-out-envelope ()
  (lambda (time)
    (min 1.0 (max 0.0 (- 1.0 time)))))

(defun f-wave (f curve sample-rate duration envelope)
  (flet ((float-to-s16 (f)
           (assert (<= -1.0 f 1.0))
           (if (plusp f)
               (floor (* f (- (expt 2 15) 1)))
               (floor (* f (expt 2 15))))))
    (let* ((block-align 2)
           (total-samples (ceiling (* sample-rate duration)))
           (size (* total-samples block-align))
           (data (make-array size :element-type '(unsigned-byte 8))))
      (loop :for i :from 0 :below size :by block-align
            :do (setf (nibbles:sb16ref/le data i)
                      (float-to-s16
                       (* (funcall f (/ (* i pi (funcall curve (/ i size))) sample-rate))
                          (funcall envelope (/ i size))))))

      data)))

(defun make-sound (curve duration instrument envelope)
  (let ((sample-rate 4000))
    (load-sound-from-wave
     (make-wave-from-memory ".wav"
                            (wav:make-wav :channels 1
                                          :sample-rate sample-rate
                                          :bits-per-sample 16
                                          :data (f-wave instrument curve sample-rate duration envelope))))))

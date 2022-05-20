(defpackage :wav
  (:use :common-lisp)
  (:export #:make-wav))

(in-package :wav)

(defun wav-write-vector (vector stream)
  (loop :for elem :across vector
        :do (vector-push elem stream)))

(defun wav-write-string (string stream)
  (wav-write-vector (map 'vector #'char-code string) stream))

(defun write-u16 (n stream)
  (vector-push (ldb (byte 8 0) n) stream)
  (vector-push (ldb (byte 8 8) n) stream))

(defun write-u32 (n stream)
  (vector-push (ldb (byte 8 0) n) stream)
  (vector-push (ldb (byte 8 8) n) stream)
  (vector-push (ldb (byte 8 16) n) stream)
  (vector-push (ldb (byte 8 24) n) stream))

(defun make-wav (&key
                   (compression-code 1)
                   (channels 1)
                   (sample-rate 44100)
                   (bits-per-sample 16)
                   (block-align (* (floor (/ bits-per-sample 8)) channels))
                   (average-bytes-per-second (* sample-rate block-align))
                   data)
  (let* ((file-size (+ 8 ;; RIFF + size
                       4 ;; WAVE
                       8 ;; 'fmt ' + size
                       16 ;; size of fmt
                       8 ;; 'data' + size
                       (length data)))
         (wav-file (make-array file-size
                               :fill-pointer 0
                               :element-type '(unsigned-byte 8))))
    (labels ((write-wav-format (stream)
               (write-u16 compression-code stream) ;; PCM compression
               (write-u16 channels stream) ;; channels
               (write-u32 sample-rate stream) ;; sample rate
               (write-u32 average-bytes-per-second stream) ;; average bytes per second
               (write-u16 block-align stream) ;; block align
               (write-u16 bits-per-sample stream))) ;; bits per sample
      (wav-write-string "RIFF" wav-file)
      (write-u32 (- file-size 8) wav-file)
      (wav-write-string "WAVE" wav-file)
      (wav-write-string "fmt " wav-file)
      (write-u32 16 wav-file)
      (write-wav-format wav-file)
      (wav-write-string "data" wav-file)
      (write-u32 (length data) wav-file)
      (wav-write-vector data wav-file))

    wav-file))

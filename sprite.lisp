(in-package :raylib)

(defun make-sprite-texture (sprite &key width height color)
  (let* ((image (gen-image-color width height +blank+))
         (image-pointer (alloc-image image)))
    (loop :for j :from 0 :below width
          :do (loop :for i :from 0 :below height
                    :do (when (progn (list i j) (= 1 (aref (aref sprite i) j)))
                          (image-draw-pixel image-pointer j i color))))
    (load-texture-from-image image)))

;(ql:quickload :cl-raylib)

(defparameter *font-size* 20)

;;; A thing's size depends on its children
;;; recursive
(defun calc-thing-size (thing &optional (padding 0)) ; => size
  (cond ((null thing)
         (cons (+ 32 (ash padding 1))
               (+ 24 (ash padding 1))))
        ((atom thing)
         (cons (raylib:measure-text (prin1-to-string thing) *font-size*)
               *font-size*))
        ((listp thing)
          (let ((first-size (calc-thing-size (first thing)))
                (rest-size (calc-thing-size (rest thing))))
            (cons (+ (+ (car first-size) (car rest-size)) (ash padding 1))
                  (+ (max (cdr first-size) (cdr rest-size)) (ash padding 1)))))
        (t
          (error "unknown thing: ~s" thing))))

(defun draw-text (text x y)
  (raylib:draw-text text x y *font-size* raylib:+black+))

(defun draw-thing (thing x y &optional (padding 0))
  (let* ((size (calc-thing-size thing padding))
         (width (first size))
         (height (rest size))
         (*steps* (1- *steps*)))
    (when (>= 0 *steps*) (return-from draw-thing))
    (raylib:draw-rectangle-lines x y
                                 width height
                                 raylib:+black+)
    (cond ((null thing)
           (raylib:draw-line (+ x width) y
                             x (+ y height)
                             raylib:+black+))
          ((listp thing)
           (do ((item thing (rest item))
                (offset padding (+ offset
                                   padding
                                   (first (calc-thing-size (first item))))))
               ((null item) nil)
             (draw-thing (first item) (+ x offset) (+ y padding))))
          ((atom thing)
           (draw-text (prin1-to-string thing) (+ x padding) (+ y padding)))
          (t
            (error "unknown thing: ~s" thing)))))

;;; rectangle contains point
(defun rectangle-contains? (rx ry width height px py)
  (and (<= rx px (+ rx width))
       (<= ry py (+ ry height))))

(defparameter *steps* 0)

(defun mainloop ()
  (setf *steps* 0)
  (raylib:with-window (500 500 "window")
    (raylib:set-target-fps 30)
    (loop
      (when (raylib:window-should-close) (return))

      (let* ((pos (raylib:get-mouse-position))
             (mx (floor (raylib:vector2-x pos)))
             (my (floor (raylib:vector2-y pos))))

        (when (raylib:is-mouse-button-pressed raylib:+mouse-left-button+)
          (incf *steps*))
        (when (raylib:is-mouse-button-pressed raylib:+mouse-right-button+)
          (decf *steps*))

      (raylib:with-drawing
        (raylib:clear-background raylib:+raywhite+)

        (draw-thing '() 20 70)
        (draw-thing '(a) 20 100)
        (draw-thing '(a b) 20 130)
        (draw-thing '(a b c) 20 160))))))

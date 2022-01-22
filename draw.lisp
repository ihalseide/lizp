;(ql:quickload :cl-raylib)

(defparameter *thing-margin* 5)

(defparameter *thing-id* 0)
(defun make-thing (content &optional x y vertical)
  (list :content content
		:x x
		:y y
		:width nil   ; updated by drawing
		:height nil  ; updated by drawing
		:vertical vertical
		:id (incf *thing-id*)))
(defun thing-content (thing) (getf thing :content))
(defun thing-x (thing) (getf thing :x))
(defun thing-y (thing) (getf thing :y))
(defun thing-width (thing) (getf thing :width))
(defun thing-height (thing) (getf thing :height))
(defun thing-vertical (thing) (getf thing :vertical))
(defun thing-id (thing) (getf thing :id))
(defun thing= (t1 t2) (= (thing-id t1) (thing-id t2)))
(defun thing-update-pos (thing x y)
  (setf (getf thing :x) x)
  (setf (getf thing :y) y))
(defun thing-update-size (thing width height)
  (setf (getf thing :width) width)
  (setf (getf thing :height) height))

(defun make-size (w h) (cons w h))
(defun width (size) (first size))
(defun height (size) (rest size))
(defun size+ (size w h)
  (make-size (+ (width size) w)
			 (+ (height size) h)))

(defun draw-text (text x y &optional (font-size 20) (color raylib:+black+)) ; => size
  (raylib:draw-text text x y font-size color)
  (make-size (raylib:measure-text text font-size) font-size))

(defun draw-nil (x y width height)
  (raylib:draw-rectangle-lines x y width height raylib:+black+)
  (raylib:draw-line (+ x width) y x (+ y height) raylib:+black+)
  (make-size width height))

;; note: padding in this case means the padding between elements
(defun draw-list (lst x y vertical padding) ; => size
  (if lst
	(let ((thing-size (draw-thing (first lst) x y)))
	  (if vertical
		(let ((rest-size
				(draw-list (rest lst) x (+ y (height thing-size) padding) vertical padding)))
		  (make-size (max (width thing-size) (width rest-size))
					 (+ (height thing-size) (height rest-size))))
		(let ((rest-size
				(draw-list (rest lst) (+ x (width thing-size) padding) y vertical padding)))
		  (make-size (+ (width thing-size) (width rest-size))
					 (max (height thing-size) (height rest-size))))))
	(if vertical
	  (size+ (draw-nil x y 32 24) 0 (* 2 padding))
	  (size+ (draw-nil x y 24 32) (* 2 padding) 0))))

(defun draw-atom (item x y)
  (when (not (or (numberp item)
				 (symbolp item)
				 (stringp item)))
	(error "unknown item: ~s" item))
  (draw-text (prin1-to-string item) x y))

(defun update-thing (thing &optional (x0 0) (y0 0))
  (if (or (null (thing-x thing))
		  (null (thing-y thing)))
	(thing-update-pos thing x0 y0))
  (let ((content (thing-content thing)))
	(when (listp content)
	  (dolist (child content) ; remember, `dolist' on nil does nothing
		(update-thing child (thing-x thing) (thing-y thing))))))

(defun draw-thing (thing &optional (x0 0) (y0 0) (outline-color raylib:+black+))
  (let* ((margin-x 8)
		 (margin-y 8)
		 (pad-list 5)
		 (content (thing-content thing))
		 (tx (or (thing-x thing) 0))
		 (ty (or (thing-y thing) 0))
		 (size (if (listp content)
				 (draw-list content (+ tx margin-x x0) (+ ty margin-y y0) (thing-vertical thing) pad-list)
				 (draw-atom content (+ tx margin-x x0) (+ ty margin-y y0))))
		 (outline (size+ size (* 2 margin-x) (* 2 margin-y))))

	;; not related to drawing
	(thing-update-size thing (width outline) (height outline))

	(raylib:draw-rectangle-lines (+ tx x0) (+ ty y0) (width outline) (height outline) outline-color)
	outline))

(defun intersects (thing x y)
  (if (and (thing-width thing)
		   (thing-height thing))
	(and (< (thing-x thing) x (+ (thing-x thing) (thing-width thing)))
		 (< (thing-y thing) y (+ (thing-y thing) (thing-height thing))))))

;;;; recursively find the deepest nested thing that contains a point
(defun find-thing-at (things x y &optional parent) ; => (thing . parent)
  (declare (type list things parent))
  (let ((thing1 (if things (first things))))
	(cond ((not things)
		   (cons nil parent))
		  ((intersects thing1 x y)
		   (let ((content (thing-content thing1)))
			 (cond ((or (atom content)
						(null content))
					(cons thing1 parent))
				   (t
					 (find-thing-at content x y thing1)))))
		  (t
			(find-thing-at (rest things) x y parent)))))

(defparameter *things* (list (make-thing '() 20 20 t)
							 (make-thing 5 40 20 t)
							 (make-thing (list (make-thing 'a)
											   (make-thing 'b)) 80 20)))
(defparameter *grab-thing* nil) ; thing grabbed by mouse
(defparameter *grab-x* nil)     ; x offset of mouse within grabbed thing
(defparameter *grab-y* nil)     ; y offset of mouse within grabbed thing

(defun mainloop ()
  (raylib:with-window (500 500 "window")
	(raylib:set-target-fps 30)
	(loop
	  (when (raylib:window-should-close) (return))

	  (let* ((pos (raylib:get-mouse-position))
			 (mx (floor (raylib:vector2-x pos)))
			 (my (floor (raylib:vector2-y pos))))

		(when (raylib:is-mouse-button-pressed raylib:+mouse-left-button+)
		  (cond (*grab-thing*
				  ;; place down the current dragged thing
				  (thing-update-pos *grab-thing* (- mx *grab-x*) (- my *grab-y*))
				  (push *grab-thing* *things*)
				  (setf *grab-thing* nil))
				;; no current dragged thing, so try to find one
				(t
				  (let* ((thing&parent (find-thing-at *things* mx my))
						 (found (first thing&parent))
						 (parent (or (rest thing&parent)
									 *things*)))
					(when found
					  (setf *grab-thing* found)
					  (setf *grab-x* (- mx (thing-x *grab-thing*)))
					  (setf *grab-y* (- my (thing-y *grab-thing*)))
					  ;; Remove the grabbed thing from the respective parent
					  (let ((parent-without (remove *grab-thing* parent :test #'thing=)))
						;; Note: this is probably not how your supposed to replace the list contents of a variable!
						;; I just want to change the value that `parent' refers to.
						(rplaca parent (first parent-without))
						(rplacd parent (rest parent-without)))
					  (thing-update-pos *grab-thing* nil nil)))))
		  (format t "~&number of *thing*s = ~a" (length *things*)))

	  (raylib:with-drawing
		(raylib:clear-background raylib:+raywhite+)

		(dolist (thing *things*)
		  (update-thing thing))

		(dolist (thing *things*)
		  (draw-thing thing))

		(when *grab-thing*
			(draw-thing *grab-thing* (- mx *grab-x*) (- my *grab-y*))))))))

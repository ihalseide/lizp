;(ql:quickload :cl-raylib)

(defparameter *font-size* 20)

(defparameter *thing-id* 0)

(defclass thing ()
  ((content
	 :initarg :content
	 :initform (error "content is required"))
   (x
	 :initarg :x
	 :initform nil)
   (y
	 :initarg :y
	 :initform nil)
   (width
	 :initform nil)
   (height
	 :initform nil)
   (vertical
	 :initarg :vertical
	 :initform nil)
   (id
	 :initform (incf *thing-id*))))

(defmethod initialize-instance :after ((th thing) &key)
  (let ((size (calc-thing-size (slot-value th 'content)
							   (slot-value th 'vertical))))
	(setf (slot-value th 'width) (width size))
	(setf (slot-value th 'height) (height size))))

(defmethod make-thing (content &optional x y vertical)
  (make-instance 'thing :content content :x x :y y :vertical vertical))

(defun thing= (t1 t2)
  (= (slot-value t1 'id)
	 (slot-value t2 'id)))

(defun size-pack (width1 height1 width2 height2 vertical)
  (if vertical
	(cons (max width1 width2)
		  (+ height1 height2))
	(cons (+ width1 width2)
		  (max height1 height2))))

;;; A thing's size depends on its children
;;; recursive
(defun calc-thing-size (content vertical) ; => size
  (cond ((null content)
		 (if vertical
		   (make-size 32 24)
		   (make-size 24 32)))
		((atom content)
		 (cons (raylib:measure-text (prin1-to-string content) *font-size*)
			   *font-size*))
		(t ; list (recursive case)
		  (size-pack (calc-thing-size (first content) vertical)
					 (calc-thing-size (rest content) vertical)
					 vertical))))

;;; A thing's position depends on its parents
;;; x,y = parent-x, parent-y
;;; this traverses through children and sets their positions
;;; recursive
(defun update-thing-position (thing x y) ; => thing
  (let ((x (or x (slot-value thing 'x)))
		(y (or y (slot-value thing 'y))))
	(setf (slot-value thing 'x) x)
	(setf (slot-value thing 'y) y)
	(let ((content (slot-value thing 'content)))
	(if (and (listp content) content)
	  (let ((child (first content)))
		(update-thing-position child x y)
		(if (slot-value thing 'vertical)
		  (update-thing-position (rest content) x (+ y (slot-value child 'height)))
		  (update-thing-position (rest content) (+ x (slot-value child 'width) y))))))))

(defun draw-text (text x y)
  (raylib:draw-text text x y *font-size* raylib:+black+))

(defun draw-atom (item x y)
  (draw-text (prin1-to-string item) x y))

(defun draw-nil (x y width height)
  (raylib:draw-rectangle-lines x y width height raylib:+black+)
  (raylib:draw-line (+ x width) y
					x (+ y height)
					raylib:+black+))

;;; draw-thing and draw-list are mutually recursive
(defun draw-list (lst x y vertical)
  (if lst
	(progn
	  (if (atom (first lst))
		(draw-atom (first lst) x y)
		(draw-thing (first lst) x y))
	  (if vertical
		(draw-list (rest lst) x (+ y (height thing-size)) vertical)
		(draw-list (rest lst) (+ x (width thing-size)) y vertical)))
	(if vertical
	  (draw-nil x y 32 24)
	  (draw-nil x y 24 32))))

;;; draw-thing and draw-list are mutually recursive
(defun draw-thing (thing &optional (x0 0) (y0 0))
  (let* ((content (slot-value thing 'content))
		 (tx (or (slot-value thing 'x) 0))
		 (ty (or (slot-value thing 'y) 0)))
	(if (listp content)
	  (draw-list content (+ tx x0) (+ ty y0) (slot-value thing 'vertical) pad-list)
	  (draw-atom content (+ tx x0) (+ ty y0)))
	(raylib:draw-rectangle-lines (+ tx x0) (+ ty y0)
								 (slot-value thing 'width)
								 (slot-value thing 'height)
								 raylib:+black+)))

;;; rectangle contains point
(defun rectangle-contains? (rx ry width height px py)
  (and (<= rx px (+ rx width))
	   (<= ry py (+ ry height))))

;;; thing contains point
(defun thing-contains? (thing px py)
  (rectangle-contains? (slot-value thing 'x)
					   (slot-value thing 'y)
					   (slot-value thing 'width)
					   (slot-value thing 'height)
					   px py))

;;;; recursively find the deepest nested thing that contains a point
(defun find-thing-at (things x y &optional parent) ; => (thing . parent)
  (declare (type list things parent))
  (let ((thing1 (if things (first things))))
	(cond ((not things)
		   (cons nil parent))
		  ((thing-contains? thing1 x y)
		   (let ((content (slot-value thing1 'content)))
			 (cond ((or (atom content)
						(null content))
					(cons thing1 parent))
				   (t
					 (find-thing-at content x y thing1)))))
		  (t
			(find-thing-at (rest things) x y parent)))))

(defparameter *things* (list (make-thing 5 20 20)))
(print *things*)
(terpri)

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
					  (setf *grab-x* (- mx (slot-value *grab-thing* 'x)))
					  (setf *grab-y* (- my (slot-value *grab-thing* 'y)))
					  ;; Remove the grabbed thing from the respective parent
					  (let ((parent-without (remove *grab-thing* parent :test #'thing=)))
						;; Note: this is probably not how your supposed to replace the list contents of a variable!
						;; I just want to change the value that `parent' refers to.
						(rplaca parent (first parent-without))
						(rplacd parent (rest parent-without)))
					  (update-thing-position *grab-thing* mx my))))))

	  (raylib:with-drawing
		(raylib:clear-background raylib:+raywhite+)

		(dolist (thing *things*)
		  (draw-thing thing))

		(when *grab-thing*
		  (draw-thing *grab-thing* (- mx *grab-x*) (- my *grab-y*))))))))

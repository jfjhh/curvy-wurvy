;;;;
;;;; Parametric curve utilities.
;;;; Alex Striff.
;;;;

(in-package #:parametric)

(defconstant TAU (* 2 PI))

(defparameter *num-points* 128)

(defparameter +cont-eps+ (expt 10 -6))

(defun id (x) x)

;;; x' = x cos t - y sin t
;;; y' = x sin t + y cos t
(defun parallel-lines (x y spacing angle &optional (width 500) (height 500))
  (let ((c (cos (- angle)))
	(s (sin (- angle)))
	(xo (/ width 2))
	(yo (/ height 2)))
    (svg-tag "g" (list 'stroke-width 1 'stroke "#000")
      (dotimes (i (1+ (floor height spacing)))
	(let* ((step (* spacing i))
	       (x1 (+ (- xo)))
	       (y1 (+ (- yo) step))
	       (x2 (+ (+ xo)))
	       (y2 (+ (- yo) step)))
	  (svg-line
	   (+ x xo (* x1 c) (* -1 y1 s))
	   (+ y yo (* x1 s) (* y1 c))
	   (+ x xo (* x2 c) (* -1 y2 s))
	   (+ y yo (* x2 s) (* y2 c))))))))

(defun grid-lines (x y spacing angle &optional (width 500) (height 500))
  (parallel-lines x y spacing angle width height)
  (parallel-lines x y spacing (+ angle (/ PI 2)) width height))

(defun point (cx cy &optional (fill "url(#paramgrad)"))
  (svg-tag "circle" (list 'stroke "none" 'fill fill 'cx cx 'cy cy 'r 2)))

(defun points->svg (points)
  (loop :for point :in points :do
     (let* ((x (car point))
	    (y (cdr point)))
       (point x y))))

(defun points->derivs (points)
  (let* ((p1 (car points))
	 (p2 (cadr points))
	 (prevx1 (car p1))
	 (prevy1 (cdr p1))
	 (prevx2 (car p2))
	 (prevy2 (cdr p2)))
    (loop :for point :in (cddr points) :collecting
       (let* ((x (car point))
	      (y (cdr point))
	      (a (- prevy1 y))
	      (b (- prevx1 x))
	      (m (unless (zerop b) (/ a b))))
	 (setf prevx1 prevx2)
	 (setf prevy1 prevy2)
	 (setf prevx2 x)
	 (setf prevy2 y)
	 m))))

(defun intersect-point (px1 py1 d1 px2 py2 d2)
  (let* ((mx  (/ (+ px1 px2) 2))
	 (my  (/ (+ py1 py2) 2))
	 (mid (or (not d1)
		  (not d2)
		  (< (abs (- d2 d1)) +cont-eps+)))
	 (a   (when d1 (* d1 px1)))
	 (cx  (if mid mx (/ (- (+ py2 a) py1 (* d2 px2)) (- d1 d2))))
	 (cy  (if mid my (- (+ py1 (* d1 cx)) a)))
	 ;;(cx  (if mid 0 (/ (- (+ py2 a) py1 (* d2 px2)) (- d1 d2))))
	 ;;(cy  (if mid 0 (- (+ py1 (* d1 cx)) a)))
	 )
    (cons cx cy)))

(defun points-derivs->controls (points derivs)
  (let* ((p1 (car points))
	 (d1 (car derivs))
	 (px1 (car p1))
	 (py1 (cdr p1))
	 (xp px1)
	 (yp py1)
	 (dp d1))
    (append
     (loop :for p2 :in (cdr points) :for d2 :in (cdr derivs) :collecting
	(let* ((px2 (car p2))
	       (py2 (cdr p2))
	       (cp  (intersect-point px1 py1 d1 px2 py2 d2)))
	  (setf d1 d2)
	  (setf px1 px2)
	  (setf py1 py2)
	  cp))
     (cons (intersect-point px1 py1 d1 xp yp dp) nil)
     ;;(cons (cons px1 py1) nil)
     )))

(defun points->controls (points)
  (points-derivs->controls points (points->derivs points)))

(defun points->points-controls (points)
  (values
   points
   (points-derivs->controls points (points->derivs points))))

(defun points-derivs->points-controls (points derivs)
  (values
   points
   (points-derivs->controls points derivs)))

(defun points-controls->path (points controls)
  (let* ((p1 (car points))
	 (px1 (car p1))
	 (py1 (cdr p1)))
    (concatenate
     'string
     (format nil "~%M ~,2f,~,2f Q" px1 py1)
     (reduce
      (lambda (a b) (concatenate 'string a b))
      (loop :for point :in (cdr points) :for cont :in controls :collecting
	 (let* ((px2 (car point))
		(py2 (cdr point))
		(cx  (car cont))
		(cy  (cdr cont))
		(str (format nil "~%~,2f,~,2f ~,2f,~,2f" cx cy px2 py2)))
	   (point px2 py2)
	   (point cx cy "#F00")
	   (setf px1 px2)
	   (setf py1 py2)
	   str))))))

(defun path->svg (path)
  (svg-tag "path" (list 'fill "transparent" 'stroke "url(#paramgrad)" 'd path)))

(defun path^2->svg (p1 p2 &optional (dur 5))
  (svg-tag "path" (list 'fill "transparent" 'stroke "url(#paramgrad)" 'd "M 0 0")
    (svg-tag "animate"
	(list "attributeName" "d"
	      "from" p1 "to" p2
	      "dur" (format nil "~,2fs" dur)
	      "repeatCount" "indefinite"))))

(defun func->points (func start end n)
  (loop :for x :from start :to end :by (/ (- end start) (1- n)) :collecting
     (cons x (funcall func x))))

(defun funcs^2->points (xfunc yfunc &optional (n *num-points*) (div 1))
  (funcs->points xfunc yfunc 0 (min 1 (/ div)) n))

(defun funcs->points (xfunc yfunc start end n)
  (loop :for x :from start :to end :by (/ (- end start) (1- n)) :collecting
     (cons (funcall xfunc x) (funcall yfunc x))))

(defun funcs^2->points-derivs (xfunc yfunc &optional (n *num-points*) (div 1))
  (funcs->points-derivs xfunc yfunc 0 (min 1 (/ div)) n))

(defun funcs->points-derivs (xfunc yfunc start end n)
  (let* ((step (/ (- end start) (1- n)))
	 (du (/ step 1e1))
	 (points nil)
	 (derivs nil))
    (loop :for u :from start :to end :by step :do
       (let* ((u*  (- u du))
	      (fx  (funcall xfunc u))
	      (fy  (funcall yfunc u))
	      (fx* (funcall xfunc u*))
	      (fy* (funcall yfunc u*))
	      (dx/du (/ (- fx* fx) du))
	      (dy/du (/ (- fy* fy) du)))
	 (setf points (cons (cons fx fy) points))
	 (setf derivs (cons (unless (< (abs dx/du) +cont-eps+) (/ dy/du dx/du)) derivs))))
    (values
     (nreverse points)
     (nreverse derivs))))

(defun points^2->svg (ps1 ps2)
  (loop :for p1 :in ps1 :for p2 :in ps2 :do
     (let ((x1 (car p1))
	   (y1 (cdr p1))
	   (x2 (car p2))
	   (y2 (cdr p2)))
       ;;(svg-line x1 y1 x2 y2)
       ;;(loop :for i :from 1/8 :to 7/8 :by 1/8 :do (point (+ (* x1 i) (* x2 (- 1 i))) (+ (* y1 i) (* y2 (- 1 i)))))
       (svg-tag "circle" (list 'stroke "none" 'fill "url(#paramgrad)" 'cx 0 'cy 0 'r 2)
	 (svg-tag "animateMotion" (list 'path (format nil "M ~,2f ~,2f L ~,2f ~,2f Z" x1 y1 x2 y2) 'dur "5s" "repeatCount" "indefinite"))))))

(defun circle->funcs^2 (cx cy &optional (radius (* (min cx cy) 3/5)))
  (values
   (lambda (x) (+ cx (* radius (cos (+ (* TAU x) (* TAU -1/4))))))
   (lambda (y) (+ cy (* radius (sin (+ (* TAU y) (* TAU -1/4))))))))

(defun neocircle->funcs^2 (cx cy &optional (radius (* (min cx cy) 3/5)))
  (values
   (lambda (x) (+ cx (* radius (cos (sin (+ (* TAU x) (* TAU -1/4)))))))
   (lambda (y) (+ cy (* radius (sin (cos (+ (* TAU y) (* TAU -1/4)))))))))

(defun segment->funcs^2 (cx cy &optional (radius (* (min cx cy) 3/5)) (angle 0))
  (values
   (lambda (x) (+ cx (* 2 radius (cos (- angle)) (+ 1/2 (- x)))))
   (lambda (y) (+ cy (* 2 radius (sin (- angle)) (+ 1/2 (- y)))))))

(defun spiral->funcs^2 (cx cy &optional (radius (* (min cx cy) 3/5)))
  (values
   (lambda (x) (+ cx (* radius x (cos (+ (* TAU TAU x))))))
   (lambda (y) (+ cy (* radius y (sin (+ (* TAU TAU y))))))))

(defun invert-curve (xfunc yfunc)
  (values
   (lambda (x) (funcall xfunc (- 1 x)))
   (lambda (y) (funcall yfunc (- 1 y)))))

(defun curve->path (curvefunc &rest args)
  (declare (type function curvefunc))
  (apply
   (multiple-value-compose
    #'points-controls->path
    #'points->points-controls
    #'funcs^2->points
    curvefunc)
   args))

#||
(defun funcs^2->path (xfunc yfunc)
  (funcall
   (multiple-value-compose
    #'points-controls->path
    #'points->points-controls
    #'funcs^2->points)
   xfunc
   yfunc))
||#

(defun funcs^2->path (xfunc yfunc)
  (funcall
   (multiple-value-compose
    #'points-controls->path
    #'points-derivs->points-controls
    #'funcs^2->points-derivs)
   xfunc
   yfunc))

(defun funcs^2^2->path (f1 args1 f2 args2)
  (path^2->svg
   (multiple-value-call #'funcs^2->path (apply f1 args1))
   (multiple-value-call #'funcs^2->path (apply f2 args2))))

(defun funcs^2^2->path* (f1 args1 f2 args2)
  (let ((p1 (multiple-value-call #'funcs^2->path (apply f1 args1)))
	(p2 (multiple-value-call #'funcs^2->path (apply f2 args2))))
    (path^2->svg p1 p2)
    (path^2->svg p2 p1)))

(defun curve->svg (curvefunc &rest args)
  (declare (type function curvefunc))
  (svg-test-curve
    (apply
     (multiple-value-compose #'points->svg #'funcs^2->points curvefunc)
     args)))

#||
(defun curve->svg* (curvefunc &rest args)
  (declare (type function curvefunc))
  (svg-test-curve
    (apply
     (multiple-value-compose
      #'path->svg
      #'points-controls->path
      #'points->points-controls
      #'funcs^2->points
      curvefunc)
     args)))
||#

(defun curve->svg* (curvefunc &rest args)
  (declare (type function curvefunc))
  (svg-test-curve
    (apply
     (multiple-value-compose
      #'path->svg
      #'points-controls->path
      #'points-derivs->points-controls
      #'funcs^2->points-derivs
      curvefunc)
     args)))

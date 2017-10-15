;;;;
;;;; SVG output language.
;;;; Alex Striff.
;;;;

(in-package #:parametric)

(defparameter *debug-stream* *standard-output*)

(defparameter *out-width* 1920)
(defparameter *out-height* 1080)

(defparameter *svg-indent* 4)
(defparameter *svg-depth* *svg-indent*)
(defparameter *svg-spaces* "")

(defparameter *text-start-x* 64)
(defparameter *text-start-y* 64)
(defparameter *text-x* *text-start-x*)
(defparameter *text-y* *text-start-y*)
(defparameter *text-size* 16)


(defmacro svg-header (&body body)
  `(progn (format t "<?xml version='1.0' encoding='UTF-8' ?>
<svg version='1.1' viewbox='0 0 ~d ~d' xmlns='http://www.w3.org/2000/svg'>~%" *out-width* *out-height*)
	  (setf *svg-indent* 4)
	  (setf *svg-depth* *svg-indent*)
	  (setf *svg-spaces* (make-string *svg-depth* :initial-element #\Space))
	  ,@body
	  (format t "</svg>~%")))

(defmacro svg-file (filename &body body)
  `(with-open-file (*standard-output* ,filename
				      :direction :output :if-exists :supersede)
     (svg-header
       ,@body)))

(defun svg-attributes (attributes &optional (prev ""))
  (if (null attributes)
      prev
      (svg-attributes (cddr attributes)
		      (concatenate 'string prev
				   (format nil " ~:[~a~;~(~a~)~]='~f'"
					   (symbolp (car attributes))
					   (car attributes)
					   (cadr attributes))))))

(defmacro svg-tag (tag attributes &body body)
  (make-string 3 :initial-element #\*)
  (if (null body)
      `(format t "~a<~a~a />~%" *svg-spaces* ,tag (svg-attributes ,attributes))
      `(progn
	 (let ((istext (equal (format nil "~(~a~)" ,tag) "text")))
	   (format t "~a<~a~a>~:[~%~;~]" *svg-spaces* ,tag (svg-attributes ,attributes) istext)
	   (incf *svg-depth* *svg-indent*)
	   (setf *svg-spaces* (make-string *svg-depth* :initial-element #\Space))
	   ,@body
	   (decf *svg-depth* *svg-indent*)
	   (setf *svg-spaces* (make-string *svg-depth* :initial-element #\Space))
	   (format t "~:[~a~;~*~]</~a>~%" istext *svg-spaces* ,tag)))))

(defun svg-comment (text)
  (format t "<!--~%~a~&-->~%" text))

(defun svg-text (text &rest attributes)
  (svg-tag "text" (append (list 'x *text-x* 'y *text-y*) attributes)
    (format t "~a" text))
  (incf *text-y* (* 2 *text-size*)))

(defgeneric svg-curve-text (curve)
  (:documentation "Outputs SVG tags that give mostly textual information about the given curve."))

(defun svg-line (x1 y1 x2 y2)
  (svg-tag "line" (list 'x1 x1 'y1 y1 'x2 x2 'y2 y2)))

(defmacro svg-test-curve (&body body)
  `(svg-file "~/tmp/test.svg"
     (svg-tag "defs" nil
       (svg-tag "linearGradient" (list 'id "paramgrad")
	 (svg-tag "stop" (list 'offset "0%"
			       'stop-color "#268bd2" 'stop-opacity 1))
	 (svg-tag "stop" (list 'offset "100%"
			       'stop-color "#6c71c4" 'stop-opacity 1))))
     ,@body))


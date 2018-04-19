(in-package :gsk-util)

#||

Functions accepting colors can take a list of
1, 3 or 4 components.

;;; Draw modes for arc
:open
:chord
:pie

;;; Draw modes for text
:left
:center
:right
:top
:bottom
:baseline

;;; Color => ok
(no-fill)
(no-stroke)
(background color)
(stroke color)
(stroke-weight weight)
(fill color)
(with-stroke-color color &body)
(with-fill-color color &body)
(with-stroke-weight weight &body)

;;; Window
(size w h)
(frame-rate)

;;; Transform
(rotate angle) => ok
(translate position) ;; list of 2 values. Maybe vec2+generics?
(scale factors) ;; x and y

;;; Primitives
(line begin end) ;; begin and end contain x and y of each point
(ellipse position size)
(arc position size start-angle stop-angle &optional (mode :open))
(rect position size &optional (corner-radius nil)) ; either one number or list of 4 radius
(triangle fst-vertex snd-vertex trd-vertex)

;;; Text
(text-align mode &optional (y-mode nil)) ; if y-mode is provided, mode is x-mode
(text-size size)
(text string position)

;;; Misc
(draw-box &key width height)
(get-distance fst-point snd-point) ; really necessary? Better avoid sqrts
;; there also was a random function which might be unecessary. Maybe a macro if needed?


;; also wondering about some texture/atlas/animation utils, should come in handy

||#


;;; Coloring
(defparameter *fill-state* t)
(defparameter *stroke-state t)
(defparameter *fill-color* '(1 1 1 1))
(defparameter *stroke-color* '(1 1 1 1))


(defmacro normalize (color-component)
  `(/ ,color-component 255.0))

(defun normalize-color-list (color)
  (values (normalize (car color))
          (normalize (cadr color))
          (normalize (caddr color))
          (if (>= (list-length color) 4)
              (normalize (cadddr color))
              1)))

(defmacro destructure-color (color)
  `(values (car ,color)
	   (cadr ,color)
	   (caddr ,color)
	   (cadddr ,color)))



(defun no-fill ()
  (setf *fill-state* nil))

(defun no-stroke ()
  (setf *stroke-state* nil))

(defmethod background ((color number))
  (let ((normalized-color (normalize color)))
    (gl:clear-color normalized-color
                    normalized-color
                    normalized-color
                    1)))

(defmethod background ((color list))
  (multiple-value-bind (r g b a)
      (normalize-color-list color)
    (gl:clear-color r g b a)))

(defmethod stroke ((color number))
  (let ((normalized-color (normalize color)))
    (setf *stroke-state* t)
    (setf *stroke-color* (list normalized-color
                               normalized-color
                               normalized-color
                               1))))

(defmethod stroke ((color list))
  (multiple-value-bind (r g b a)
      (normalize-color-list color)
    (setf *stroke-state* t)
    (setf *stroke-color* (list r g b a))))

(defmacro with-stroke-color (color &body body)
  `(progn (stroke ,color)
          ,@body
          (no-stroke)))

(defun stroke-weight (weight)
  (gl:line-width weight))

(defmacro with-stroke-weight (weight &body body)
  `(progn (stroke-weight ,weight)
          ,@body
          (stroke-weight 1)))

(defmethod fill-primitive ((color number))
  (let ((normalized-color (normalize color)))
    (setf *fill-state* t)
    (setf *fill-color* (list normalized-color
                             normalized-color
                             normalized-color
                             1))))

(defmethod fill-primitive ((color list))
  (multiple-value-bind (r g b a)
      (normalize-color-list color)
    (setf *fill-state* t)
    (setf *fill-color* (list r g b a))))

(defmacro with-fill-color (color &body body)
  `(progn (fill-primitive ,color)
          ,@body
          (no-fill)))

;;; Transform
(defmacro deg-to-rad (deg-angle)
  `(/ (* ,deg-angle pi) 180.0))

(defmacro rad-to-deg (rad-angle)
  `(/ (* ,rad-angle 180.0) pi))



(defun transform-rotate (angle)
  (gl:rotate (rad-to-deg angle) 0 0 1))


(defmacro destructure-coordinates (list)
  `(values (car ,list)
	   (cadr ,list)
	   (caddr ,list)))

(defmethod transform-translate ((position list))
  (multiple-value-bind (x y z)
      (destructure-coordinates position)
    (declare (ignore z))
    (gl:translate x y 0)))

(defmethod transform-scale ((factors list))
  (multiple-value-bind (x y z)
      (destructure-coordinates factors)
    (declare (ignore z))
    (gl:scale x y 1)))




;;; Primitives
(defmacro vertex2d (coordinate)
  `(multiple-value-bind (x y z)
       (destructure-coordinates ,coordinate)
     (declare (ignore z))
     (gl:vertex x y 0)))

(defmacro set-color (color)
  `(multiple-value-bind (r g b a)
       (destructure-color ,color)
     (gl:color r g b a)))


(defmethod line ((begin list) (end list))
  (when *stroke-state*
    (gl:polygon-mode :FRONT-AND-BACK :LINE)
    (set-color *stroke-color*)
    (gl:with-primitives :LINES
      (vertex2d begin)
      (vertex2d end))
    (gl:polygon-mode :FRONT-AND-BACK :FILL)))

(defun raw-ellipse (position size)
  (gl:with-pushed-matrix
    (transform-translate position)
    (multiple-value-bind (width height)
	(destructure-coordinates size)
      (let ((half-w (/ width 2.0))
	    (half-h (/ height 2.0)))
	(gl:with-primitive :POLYGON
	  (loop for i from 0 to 360
	     do (let ((rad (deg-to-rad i)))
		  (vertex2d (list (* (cos rad) half-w)
				  (* (sin rad) half-h))))))))))

(defmethod ellipse ((position list) (size list))
  (when *fill-state*
    (gl:polygon-mode :FRONT-AND-BACK :FILL)
    (set-color *fill-color*)
    (raw-ellipse position size))
  (when *stroke-state*
    (gl:polygon-mode :FRONT-AND-BACK :LINE)
    (set-color *stroke-color*)
    (raw-ellipse position size))
  (gl:polygon-mode :FRONT-AND-BACK :FILL))


;; Can probably be optimized
(defun raw-arc (position size start stop mode is-stroke)
  (when (>= stop start)
    (if is-stroke
	;; Stroke mode case
	(gl:with-pushed-matrix
	    (transform-translate position)
	  (let ((deg-init (rad-to-deg start))
		(deg-end (rad-to-deg stop))
		(half-w (/ (car size) 2.0))
		(half-h (/ (cadr size) 2.0)))
	    (gl:with-primitives (case mode
				  (:OPEN :LINE-STRIP)
				  (otherwise :LINE-LOOP))
	      (loop for i from deg-init to deg-end
		 do (let ((rad (deg-to-rad i)))
		      (vertex2d (list (* (cos rad)
					 half-w)
				      (* (sin rad)
					 half-h)))))
	      ;; If pie, draw a final vertex
	      (when (eq mode :PIE)
		(vertex2d '(0 0))))))
	;; Fill mode case
        (gl:with-pushed-matrix
	    (transform-translate position)
	  (gl:with-primitives (case mode
				((:OPEN :CHORD) :POLYGON)
				(otherwise :TRIANGLE_FAN))
	    (let ((deg-init (rad-to-deg start))
		  (deg-end (rad-to-deg stop))
		  (half-w (/ (car size) 2.0))
		  (half-h (/ (cadr size) 2.0)))
	      (loop for i from deg-init to deg-end
		 initially (when (eq mode :PIE)
			     (vertex2d '(0 0)))
		 do (let ((rad (deg-to-rad i)))
		      (vertex2d (list (* (cos rad)
					 half-w)
				      (* (sin rad)
					 half-h)))))
	      (when (or (eq mode :OPEN)
		       (eq mode :CHORD))
		(vertex2d '(0 0)))))))))

(defmethod arc ((position list) (size list) start-angle stop-angle &optional (mode :OPEN))
  (when (or (eq mode :OPEN)
	   (eq mode :CHORD)
	   (eq mode :PIE))
    (when *fill-state*
      (gl:polygon-mode :FRONT-AND-BACK :FILL)
      (set-color *fill-color*)
      (raw-arc position size start-angle stop-angle mode nil))
    (when *stroke-state*
      (gl:polygon-mode :FRONT-AND-BACK :LINE)
      (set-color *stroke-color*)
      (raw-arc position size start-angle stop-angle mode t))
    (gl:polygon-mode :FRONT-AND-BACK :FILL)))

;;(rect position size &optional (corner-radius nil))

(defun raw-rect (position size)
  (gl:with-pushed-matrix
    (transform-translate position)
    (multiple-value-bind (width height)
	(destructure-coordinates size)
      (gl:with-primitives :QUADS
	(vertex2d '(0 0))
	(vertex2d (list width 0))
	(vertex2d (list width height))
	(vertex2d (list 0 height))))))

(defmethod rect ((position list) (size list) &optional (corner-radius nil))
  (ccase (list-length corner-radius)
    (0 (progn
	 (when *fill-state*
	   (gl:polygon-mode :FRONT-AND-BACK :FILL)
	   (set-color *fill-color*)
	   (raw-rect position size))
	 (when *stroke-state*
	   (gl:polygon-mode :FRONT-AND-BACK :LINE)
	   (set-color *stroke-color*)
	   (raw-rect position size))))
    ;; TODO: 1 and 3
    ))


(defun raw-triangle (first second third)
  (gl:with-pushed-matrix
    (gl:with-primitives :TRIANGLES
      (vertex2d first)
      (vertex2d second)
      (vertex2d third))))

(defmethod triangle ((first list) (second list) (third list))
  (when *fill-state*
    (gl:polygon-mode :FRONT-AND-BACK :FILL)
    (set-color *fill-color*)
    (raw-triangle first second third))
  (when *stroke-state*
    (gl:polygon-mode :FRONT-AND-BACK :LINE)
    (set-color *stroke-color*)
    (raw-triangle first second third))
  (gl:polygon-mode :FRONT-AND-BACK :FILL))


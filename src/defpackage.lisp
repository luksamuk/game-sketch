(defpackage gsk
  (:use :cl)
  (:export :next-frame
           :add-update-callback
           :add-draw-callback
           :reset-callbacks
           :run-sketch))

(defpackage gsk-util
  (:use :cl)
  (:export :*fill-state*
	   :*stroke-state*
	   :*fill-color*
	   :*stroke-color*
           :no-fill
           :no-stroke
           :background
           :stroke
           :with-stroke-color
           :stroke-weight
           :with-stroke-weight
           :fill-primitive
           :with-fill-color
	   :deg-to-rad
	   :rad-to-deg
	   :transform-rotate
	   :transform-translate
	   :transform-scale
	   :line
	   :ellipse
	   :arc
	   :triangle))
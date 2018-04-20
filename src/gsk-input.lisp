(in-package :gsk-input)


(defparameter *current-state* '((:up nil)
				(:down nil)
				(:left nil)
				(:right nil)
				(:start nil)
				(:a nil)
				(:b nil)
				(:x nil)
				(:y nil)))

(defparameter *previous-state* (copy-list *current-state*))

(defun set-button (button state)
  (setf (cdr (assoc button *current-state*))
	state))

(defun flip-state ()
  (setf *previous-state* (copy-list *current-state*)))

(defun pressingp (button)
  (cdr (assoc button *current-state*)))

(defun pressedp (button)
  (and (cdr (assoc button *current-state*))
     (not (cdr (assoc button *previous-state*)))))

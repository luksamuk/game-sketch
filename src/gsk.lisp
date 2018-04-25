(in-package :gsk)


(defparameter *sketch-title* "game-sketch")
(defparameter *window* nil)
(defparameter *gl-context* nil)
(defparameter *window-width* 960)
(defparameter *window-height* 540)
(defparameter *game-controllers* nil)
(defparameter *running* t)

(defvar *next-frame-hook-mutex* (bt:make-lock "frame-hook-lock"))
(defparameter *next-frame-hook* nil)

(defparameter *sketch-setup-hook* nil)
(defparameter *sketch-update-hook* nil)
(defparameter *sketch-draw-hook* nil)


(defmacro restartable (&body body)
  `(restart-case
       (progn ,@body)
     (continue () :report "Continue")))



(defmacro next-frame (&body body)
  "Executes a command and calls the next frame."
  `(bt:with-lock-held (*next-frame-hook-mutex*)
     (progn (push (lambda () ,@body) *next-frame-hook*))))

(defun add-setup-callback (setup-function)
  "Adds a setup function to the setup hook."
  (push setup-function *sketch-setup-hook*))

(defun add-update-callback (update-function)
  "Adds an update function to the update hook."
  (push update-function *sketch-update-hook*))

(defun add-draw-callback (draw-function)
  "Adds a draw function to the draw hook."
  (push draw-function *sketch-draw-hook*))

(defun reset-callbacks ()
  "Removes all function callbacks on all hooks."
  (setf *sketch-update-hook* nil)
  (setf *sketch-draw-hook* nil))



(define-condition no-window () ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (princ "Cannot continue; no window created" stream))))


(defun setup-view ()
  (when (string= (software-type) "Win32")
    (sdl2:hide-window *window*)
    (sdl2:show-window *window*))
  (gl:enable :BLEND)
  (gl:blend-func :SRC-ALPHA :ONE-MINUS-SRC-ALPHA)
  (gl:enable :DEPTH-TEST)
  (gl:depth-func :LEQUAL)
  (gl:viewport 0 0 *window-width* *window-height*)
  (gl:matrix-mode :projection)
  (gl:ortho 0 *window-width* *window-height* 0 0 1)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (gl:clear-color 0.0 0.0 0.0 1.0))

(defun init ()
  (gsk-input:set-button :up nil)
  (gsk-input:set-button :down nil)
  (gsk-input:set-button :left nil)
  (gsk-input:set-button :right nil)
  (gsk-input:set-button :a nil)
  (gsk-input:set-button :b nil)
  (gsk-input:set-button :x nil)
  (gsk-input:set-button :y nil)
  (gsk-input:set-button :start nil)
  (gsk-input:flip-state))

(defun sketch-runner ()
  (sdl2:with-init (:video :joystick)
    (setf *window*
          (sdl2:create-window :title *sketch-title*
                              :w *window-width*
                              :h *window-height*
                              :flags '(:shown :opengl)))
    (when (null *window*)
      (error 'no-window))
    (setf *gl-context* (sdl2:gl-create-context *window*))
    (sdl2:gl-make-current *window* *gl-context*)
    (setf cl-opengl-bindings:*gl-get-proc-address*
          #'sdl2:gl-get-proc-address)
    ;;(setf (sdl2:frame-rate) 60)
    (let ((previous-tick (sdl2:get-ticks)))
      (setup-view)
      (init)
      (loop for setup-function in *sketch-setup-hook*
	 do (funcall setup-function))
      (sdl2:with-event-loop (:method :poll)
        (:quit () t)
	;; Mappings:
	;; WASD  = Directionals
	;; IJKL  = YXAB
	;; Enter = Start
        (:keyup (:keysym keysym)
		(when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-w)
		  (gsk-input:set-button :up nil))
		(when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-a)
		  (gsk-input:set-button :left nil))
		(when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-s)
		  (gsk-input:set-button :down nil))
		(when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-d)
		  (gsk-input:set-button :right nil))
		(when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-i)
		  (gsk-input:set-button :y nil))
		(when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-j)
		  (gsk-input:set-button :x nil))
		(when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-k)
		  (gsk-input:set-button :a nil))
		(when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-l)
		  (gsk-input:set-button :b nil))
		(when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-return)
		  (gsk-input:set-button :start nil)))
        (:keydown (:keysym keysym)
                  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-w)
		    (gsk-input:set-button :up t))
		  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-a)
		    (gsk-input:set-button :left t))
		  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-s)
		    (gsk-input:set-button :down t))
		  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-d)
		    (gsk-input:set-button :right t))
		  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-i)
		    (gsk-input:set-button :y t))
		  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-j)
		    (gsk-input:set-button :x t))
		  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-k)
		    (gsk-input:set-button :a t))
		  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-l)
		    (gsk-input:set-button :b t))
		  (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-return)
		    (gsk-input:set-button :start t)))

	;; No joy input yet
        (:joyaxismotion (:which controller-id :axis axis-id :value value)
                        (declare (ignore controller-id axis-id value)))
        (:joybuttondown (:which controller-id :button button-id)
                        (declare (ignore controller-id button-id)))
        (:joybuttonup (:which controller-id :button button-id)
                      (declare (ignore controller-id button-id)))
	
        (:idle ()
               #+(and sbcl (not sb-thread))
               (restartable (sb-sys:serve-all-events 0))
               (let* ((dt-proto (- (sdl2:get-ticks)
                                   previous-tick))
                      (delta-time (if (minusp dt-proto)
                                      0
                                      dt-proto)))
                 (setf previous-tick (sdl2:get-ticks))

		 (loop for update-function in *sketch-update-hook*
                    do (restartable (funcall update-function delta-time))))

               (gl:clear :color-buffer :depth-buffer)
               (loop for draw-function in *sketch-draw-hook*
                  do (restartable (funcall draw-function))
		  finally (gsk-input:flip-state))
               (sdl2:gl-swap-window *window*)
                                    
               (bt:with-lock-held (*next-frame-hook-mutex*)
                 (loop for i in *next-frame-hook*
                    do (funcall i))
                 (setf *next-frame-hook* nil))))
      (loop for (i . controller) in *game-controllers*
         do (sdl2:game-controller-close controller))
      ;; TODO HERE: Delete created texture
      (sdl2:gl-delete-context *gl-context*)
      (sdl2:destroy-window *window*))))



(defun run-sketch ()
  "Runs the sketch."
  ;; TODO: Multithreaded. For now, eval it.
  (sketch-runner))

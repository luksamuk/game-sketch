# game-sketch

A system for creating sketches, inspired by the Processing 2.x API.

## License
This software is distributed under the MIT License. See `LICENSE` for details.

## Installing

Before using `game-sketch`, make sure you have the following dependencies:

* OpenGL 2.x support;
* SDL2 installed.

If you're using Windows, you can drop the [SDL2 Win32 runtime library](https://www.libsdl.org/download-2.0.php)
on the same folder you've opened your REPL. If you're using Emacs+SLIME/Portacle, just drop it
on the same folder you've saved your sketch's `.lisp` files or your system.

Or you may also add the runtime to your Windows system PATH variable.

Just go to your Quicklisp folder and fetch this repo under local projects.

```bash
$ cd /path/to/quicklisp
$ cd local-projects
$ git clone https://github.com/luksamuk/game-sketch
```

You can also create a symlink to it on the `local-projects` folder, should you choose
to clone it on another directory.

## Usage
Just use ASDF to declare a dependency on `game-sketch`, or use Quicklisp instead:

```lisp
(ql:quickload 'game-sketch)
```

The sketches work by attaching your own `update` and `draw` functions to the
specific hooks -- `update` being a function that receives the difference in time,
in seconds, from the last to the current frame (`delta-time`), and `draw` being a
function which does not receive any parameters. Like this:

```lisp
(defun update (delta-time)
  (declare (ignore delta-time)))

(defun draw () )

(gsk:add-update-callback 'update)
(gsk:add-draw-callback 'draw)
```

The above code will show nothing on screen, but you can update the functions on
runtime and re-evaluate them under SLIME as you code, while you're running the sketch.

To actually run the sketch, just call the function for that:

```lisp
(gsk:run-sketch)
```

If by any chance you feel like you've messed up the callbacks, just remove everything
from all hooks by using:

```lisp
(gsk:reset-callbacks)
```

## Example
Use this code to test the system.

You can also manually type it on the REPL.

```lisp
(ql:quickload 'game-sketch)

(defparameter *rotation* 0)

(defun my-update (dt)
  (declare (ignore dt))
  (setf *rotation*
        (mod (+ *rotation*
		(/ dt 500.0))
	     360)))

(defun my-draw ()
  (gl:with-pushed-matrix
    (gsk-util:transform-translate
     (list (ash 960 -1)
	   (ash 540 -1)))
    (gsk-util:transform-rotate *rotation*)
    (gsk-util:with-stroke-color '(255 255 255)
    (gsk-util:rect '(-50 -50) '(100 100) '(10 30 50 70)))))

(gsk:add-update-callback 'my-update)
(gsk:add-draw-callback 'my-draw)

;; Either call this on REPL or eval it in parallel under SLIME
;;(gsk:run-sketch)
```

## Issues
* For now, the system is set to create a window with an exact 960x540 size.
* In my system, the GPU forces the application to use a 60FPS framerate, but the application itself doesn't
have that cap; you may want to interpolate your logic using the `delta-time` parameter of your `update` function.
* The sketches still don't perform a blending test, so it doesn't yet blend colors with an alpha component properly.
Will be solved very soon, as this is an easy fix.
* There may also be undiscovered bugs, so file up an issue if needed.

## Documentation

### Core package (`gsk`)
This package stores functions and macros related to the running sketch itself.

* `(next-frame &body body)`

[macro] Evaluates body once, on the next available frame of the application.

* `(add-update-callback update-function)`

[function] Adds `update-function` to the update functions hook.

* `(add-draw-callback draw-function)`

[function] Adds `draw-function` to the draw functions hook.

* `(reset-callbacks)`

[function] Clears all available function hooks.

* `run-sketch`

[function] Runs the current sketch as it is.

### Utils package (`gsk-util`)
This package stores functions, macros and variables related to drawing primitives.

* `*fill-state*`

[parameter] Stores the current fill state for primitives (whether they should be filled with a color or not).

* `*stroke-state*`

[parameter] Stores the current stroke state for primitives (whether they should be outlined with a color or not).

* `*fill-color*`

[parameter] A list storing the current fill color.

* `*stroke-color*`

[parameter] A list storing the current outline color.

* `(no-fill)`

[function] Forces the sketch to stop filling primitives with solid colors.

* `(no-stroke)`

[function] Forces the sketch to stop outlining primitives with solid colors.

* `(background ((color list)))`

[method] Changes the background color of the sketch to the given `color` list. The list may have three (red,
green, blue) or four (red, green, blue, alpha) components. The components are defined in a range [0-255].

* `(stroke ((color number))`

[method] Changes the outline color of the next drawn primitives to `color`. `color` is a single number in the
range [0-255], which will be used as the red, green and blue color components.

* `(stroke ((color list)))`

[method] Changes the outline color of the next drawn primitives to `color`. `color` is a list of three (red,
green, blue) or four (red, green, blue, alpha) components. Each component ranges [0-255].

* `(with-stroke-color (color &body body))`

[macro] Changes the outline color to `color` for all primitives drawn on `body`, then forces the sketch to
stop using outlines at the end. For information on how to specify a color, see the `stroke` methods.

* `(stroke-weight weight)`

[function] Forces the outline to assume a specific level of thickness of `weight`. Defaults to `1.0`.

* `(fill-primitive ((color list)))`

[method] Changes the fill color of the next drawn primitives to `color`. `color` is a list of three (red,
green, blue) or four (red, green, blue, alpha) components. Each component ranges [0-255].

* `(with-fill-color (color &body body))`

[macro] Changes the fill color to `color` for all primitives drawn on `body`, then forces the sketch to
stop filling primitives at the end. For information on how to specify a color, see the `fill-primitive` methods.

* `(deg-to-rad deg-angle)`

[macro] Converts an angle `deg-angle` in degrees to radians.

* `(rad-to-deg rad-angle)`

[macro] Converts an angle `rad-angle` in radians to degrees.

* `(transform-rotate angle)`

[function] Rotates the current matrix to `angle`, where `angle` is the angle in radians. I recommend transform
functions such as this one to be used in association with the `gl:with-pushed-matrix` macro from `cl-opengl`
system, which should already come as a dependency for this one.

* `(transform-translate ((position list)))`

[method] Translates the current matrix to `position`, where `position` is a list of two or three coordinates
(the third coordinate is ignored for now, and defaults to 0). I recommend transform functions such as this
one to be used in association with the `gl:with-pushed-matrix` macro from `cl-opengl` system, which should
already come as a dependency for this one.

* `(transform-scale ((factors list)))`

[method] Scales the current matrix by `factors`, where `factors` is a list of two or three scaling factors
(the third factor is ignored for now, and defaults to 1). I recommend transform functions such as this one to
be used in association with the `gl:with-pushed-matrix` macro from `cl-opengl` system, which should already
come as a dependency for this one.

* `(line begin end)`

[method] Draws a line between the `begin` and `end` points, where each point is a list of two coordinates
related to the origin (normally, the top-left corner of screen). If you wish to move the origin and avoid
vertex recalculations, I recommend looking at the `transform-*` functions and methods. The line is drawn
taking in consideration the stroke color.

* `(ellipse position size)`

[method] Draws an ellipse centered at `position`, and with diameters `size`. `position` should be a list of
two coordinates related to the origin (normally, the top-left corner of screen), while `size` should be a list
of two numbers, specifying the horizontal and vertical diameters for the ellipse. If you wish to move the
origin and avoid vertex recalculations, I recommend looking at the `transform-*` functions and methods.

* `(arc position size start-angle stop-angle &optional (mode :OPEN))`

[method] Draws an arc centered at `position`, with diameters `size`, like the `ellipse` method. However, it
is drawn like a pie or a pizza, which is determined by `start-angle` and `stop-angle`, both in radians. The
angles start at the left and grow counterclockwise. You may also want to specify a `mode` for drawing the
arc, which can be `:OPEN` (the default mode; only draws the outline outside of the arc), `:CHORD` (outlines
the whole shape, connecting the start and the end points), or `:PIE` (similar to `:CHORD`, but connects
the end to the center and then to the start, making a pie-like shape). If you wish to move the origin and avoid
vertex recalculations, I recommend looking at the `transform-*` functions and methods.

* `(rect position size &optional (corner-radius nil))`
[method] Draws a rectangle with the TOP-LEFT CORNER at `position`, with sides of size `size`. `position`
should be a list of two coordinates, while `size` should be a list with of two dimensions: width and height
of the rectangle. `corner-radius` is optional, and should always be a list; should it be provided, depending
on the amount of items in the list, a rectangle with rounded borders will be drawn. For example: a zero-sized
list (or `nil`) will yield a common rectangle; a list with only one number will yield a rectangle with four
rounded borders, each border with the radius specified by the single item in the list; a list with four
numbers will yield a rectangle with four rounded borders, each border having its own radius, specified by
each list element.
Radiuses should be specified by the order: top-left, top-right, bottom-left, bottom-right, respectfully.
If you wish to move the origin and avoid vertex recalculations, I recommend looking at the `transform-*`
functions and methods.

* `(triangle first second third)`

[method] Draws a triangle using the `first`, `second` and `third` vectors. The vertices should each be a list of
two coordinates related to the origin (normally, the top-left corner of screen). If you wish to move the origin and
avoid vertex recalculations, I recommend looking at the `transform-*` functions and methods.

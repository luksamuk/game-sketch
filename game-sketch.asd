;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defsystem game-sketch
  :name "game-sketch"
  :version "0.1.0"
  :maintainer "Lucas Vieira <lucasvieira@lisp.com.br>"
  :author "Lucas Vieira"
  :licence "MIT"
  :description "Game Sketch"
  :long-description "Create sketches and games. Inspired by the Processing 2.x API."
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "defpackage")
			     (:file "gsk-input")
                             (:file "gsk")
                             (:file "gsk-util"))))
  :depends-on (:sdl2
               :cl-opengl
               :bordeaux-threads))

#! /usr/bin/env racket

#lang racket/gui
(require racket/class)
(require racket/gui/base)
(require sgl)
(require sgl/gl)
(require sgl/gl-vectors)

; Macro to delimit and automatically end glBegin - glEnd contexts.
(define-syntax-rule (glbgnend Vertex-Mode statement ...)
  (let () (glBegin Vertex-Mode) statement ... (glEnd)))
 
(define (resize w h)
  (glViewport 0 0 w h))

(define (draw-opengl x y z)
  (glClearColor 0.0 0.0 0.0 0.0)
  (glEnable GL_DEPTH_TEST)
  (glClear GL_COLOR_BUFFER_BIT)
  (glClear GL_DEPTH_BUFFER_BIT)
 
  (define max-axis (add1 (max x y z)))

  (glMatrixMode GL_PROJECTION)
  (glLoadIdentity)
  ;  (glOrtho 0.0  1.0  0.0  1.0  -1.0  1.0)
;  (print (/ max-axis 2))
  (glOrtho (/ (- max-axis) 2) max-axis (/ (- max-axis) 2) max-axis (/ (- max-axis) 2) max-axis)
  (glMatrixMode GL_MODELVIEW)
  (glLoadIdentity)
  (glRotatef -5 1.0 0.0 0.0)
  (glRotatef 5 0.0 1.0 0.0)
;  (glRotatef 45 0.0 0.0 1.0)

  ;  (glBegin GL_TRIANGLES)
;  (glColor3f 1 0 0) (glVertex3d 0.25 0.25 0.0)
;  (glColor3f 0 1 0) (glVertex3d 0.75 0.25 0.0)
;  (glColor3f 0 0 1) (glVertex3d 0.75 0.75 0.0)

;  (glbgnend GL_SPHERE (glColor3f 1 1 0) (glVertex3d 0 0 0) 1.4)

;  (glbgnend gluSphere(0 0 0) )
  (glbgnend GL_QUADS
                (glColor3f 0 0 1)
                (glVertex3d x 0.0 z)
                (glVertex3d x y z)
                (glVertex3d x y 0.0)
                (glVertex3d x 0.0 0.0))
  (glbgnend GL_QUADS
                (glColor3f 1 0 0)
                (glVertex3d x 0.0 0.0)
                (glVertex3d x y 0.0)
                (glVertex3d 0.0 y 0.0)
                (glVertex3d 0.0 0.0 0.0)) 
  (glbgnend GL_QUADS
                (glColor3f 0 1 0)
                (glVertex3d x y 0.0)
                (glVertex3d x y z)
                (glVertex3d 0.0 y z)
                (glVertex3d 0.0 y 0.0)))
 
(define my-canvas%
  (class* canvas% ()
    (inherit with-gl-context swap-gl-buffers)
    (init-field (x 2) (y 3) (z 3))
    (define/override (on-paint)
      (with-gl-context
        (lambda ()
          (draw-opengl x y z)
          (swap-gl-buffers))))
    (define/override (on-size width height)
      (with-gl-context
        (lambda ()
          (resize width height))))
    (super-instantiate () (style '(gl)))
    ))
(define win (new frame% (label "Racket Draw a Rot/Zombie-Me~!") (min-width 400) (min-height 800)))
(define gl  (new my-canvas% (parent win) (x 2) (y 3) (z 3)))
 
(send win show #t)
 

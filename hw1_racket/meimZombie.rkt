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

(define vec3%
  (class object%
    (init x y z)
    (define curr-x x)
    (define curr-y y)
    (define curr-z z)
    (super-new)
    (define/public (getX) curr-x)
    (define/public (getY) curr-y)
    (define/public (getZ) curr-z)

    (define/public (setX x)
      (set! curr-x x))
    (define/public (setY y)
      (set! curr-y y))
    (define/public (setZ z)
      (set! curr-z z))

    (define/public (set v) ; vec3
      (set! curr-x (send v getX))
      (set! curr-y (send v getY))
      (set! curr-z (send v getZ)))
    ))

(define obj%
  (class object%
    (init size ori ro ax angle color coord) ; vec3: size ori (x y z)
    (define currsize size)
    (define currori ori)
    (define currro ro)
    (define currax ax)
    (define currangle angle)
    (define currcolor color)
    (define currcoord coord)
    (super-new)
    (define/public (get-size) currsize)
    (define/public (get-ori) currori)
    (define/public (get-ro) currro)
    (define/public (get-ax) currax)
    (define/public (get-angle) currangle)
    (define/public (get-color) currcolor)
    (define/public (getCoords idx)
      (vector-ref currcoord idx))
    (define/public (updateCoords size ori)
      (set! currcoord (make-vector 24)) ; size sizeo
      (vector-set! currcoord 0 (- (send ori getX) (send size getX)))  ; 0
      (vector-set! currcoord 1 (+ (send ori getY) (send size getY)))
      (vector-set! currcoord 2 (- (send ori getZ) (send size getZ)))
      (vector-set! currcoord 3 (+ (send ori getX) (send size getX)))  ; 1      
      (vector-set! currcoord 4 (+ (send ori getY) (send size getY)))
      (vector-set! currcoord 5 (- (send ori getZ) (send size getZ)))
      (vector-set! currcoord 6 (+ (send ori getX) (send size getX)))  ; 2
      (vector-set! currcoord 7 (+ (send ori getY) (send size getY)))
      (vector-set! currcoord 8 (+ (send ori getZ) (send size getZ)))
      (vector-set! currcoord 9 (- (send ori getX) (send size getX)))  ; 3
      (vector-set! currcoord 10 (+ (send ori getY) (send size getY)))
      (vector-set! currcoord 11 (+ (send ori getZ) (send size getZ)))
      (vector-set! currcoord 12 (- (send ori getX) (send size getX))) ; 4
      (vector-set! currcoord 13 (- (send ori getY) (send size getY)))
      (vector-set! currcoord 14 (- (send ori getZ) (send size getZ)))
      (vector-set! currcoord 15 (- (send ori getX) (send size getX))) ; 5
      (vector-set! currcoord 16 (- (send ori getY) (send size getY)))
      (vector-set! currcoord 17 (+ (send ori getZ) (send size getZ)))
      (vector-set! currcoord 18 (+ (send ori getX) (send size getX))) ; 6
      (vector-set! currcoord 19 (- (send ori getY) (send size getY)))
      (vector-set! currcoord 20 (+ (send ori getZ) (send size getZ)))
      (vector-set! currcoord 21 (+ (send ori getX) (send size getX))) ; 7
      (vector-set! currcoord 22 (- (send ori getY) (send size getY)))
      (vector-set! currcoord 23 (- (send ori getZ) (send size getZ)))
;      (print currcoord)
      )
    (define/public (resetSize amt) ; currcoord
      (set! currsize amt))
    (define/public (resetOri ori)  ; currcoord
      (set! currori ori))
    (define/public (resetRo ro)
      (set! currro ro))
    (define/public (resetAx ax)
      (set! currax ax))
    (define/public (resetAngle angle)
      (set! currangle angle))
    (define/public (resetColor color)
      (set! currcolor color))
    ))

(define myhead (new obj% [size (new vec3% [x 1] [y 1] [z 1])] [ori (new vec3% [x 0] [y 0] [z 0])] [ro (new vec3% [x 0] [y 2] [z 0])] [ax 3] [angle 45]
                    [color "yellow"]
                    [coord '()]
                    ))
(define tsize (new vec3% [x 1] [y 1] [z 1]))
(define tori (new vec3% [x 0] [y 0] [z 0]))
(send myhead updateCoords tsize tori)

;(define (draw obj)
;  (sphere
;   (send obj get-ori)
;   (send obj get-size)))

(define (draw-opengl x y z)
  (glClearColor 1 1 1 0.0)
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
  (glRotatef -30 1.0 0.0 0.0)
  (glRotatef -52 0.0 1.0 0.0)
  (glRotatef 60 0.0 0.0 1.0)

  (glbgnend GL_QUADS ; 0 1 2 3
            (glColor3f 0 0 1) 
            (glVertex3d (send myhead getCoords 0) (send myhead getCoords 1) (send myhead getCoords 2))
            (glVertex3d (send myhead getCoords 3) (send myhead getCoords 4) (send myhead getCoords 5))
            (glVertex3d (send myhead getCoords 6) (send myhead getCoords 7) (send myhead getCoords 8))
            (glVertex3d (send myhead getCoords 9) (send myhead getCoords 10) (send myhead getCoords 11)))
  (glbgnend GL_QUADS ; 1 7 6 2
            (glColor3f 1 0 0)
            (glVertex3d (send myhead getCoords 0) (send myhead getCoords 1) (send myhead getCoords 2))
            (glVertex3d (send myhead getCoords 21) (send myhead getCoords 22) (send myhead getCoords 23))
            (glVertex3d (send myhead getCoords 18) (send myhead getCoords 19) (send myhead getCoords 20))
            (glVertex3d (send myhead getCoords 6) (send myhead getCoords 7) (send myhead getCoords 8)))
  (glbgnend GL_QUADS ; 2 6 5 3
            (glColor3f 1 1 0)
            (glVertex3d (send myhead getCoords 9) (send myhead getCoords 10) (send myhead getCoords 11))
            (glVertex3d (send myhead getCoords 18) (send myhead getCoords 19) (send myhead getCoords 20))
            (glVertex3d (send myhead getCoords 15) (send myhead getCoords 16) (send myhead getCoords 17))
            (glVertex3d (send myhead getCoords 9) (send myhead getCoords 10) (send myhead getCoords 11))))
 
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
(define win (new frame% (label "Hello world, I am a Zombie!") (min-width 800) (min-height 800)))
(define gl  (new my-canvas% (parent win) (x 1) (y 1) (z 1)))
 
(send win show #t)
 

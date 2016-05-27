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
    (define/public (updateCoords size ori)
      (set! currcoord (make-vector 8))
      (vector-set! currcoord 0 (new vec3% [x (- (send ori getX) (send size getX))] [y (+ (send ori getY) (send size getY))] [z (- (send ori getZ) (send size getZ))]))
      (vector-set! currcoord 1 (new vec3% [x (+ (send ori getX) (send size getX))] [y (+ (send ori getY) (send size getY))] [z (- (send ori getZ) (send size getZ))]))
      (vector-set! currcoord 2 (new vec3% [x (+ (send ori getX) (send size getX))] [y (+ (send ori getY) (send size getY))] [z (+ (send ori getZ) (send size getZ))]))
      (vector-set! currcoord 3 (new vec3% [x (- (send ori getX) (send size getX))] [y (+ (send ori getY) (send size getY))] [z (+ (send ori getZ) (send size getZ))]))
      (vector-set! currcoord 4 (new vec3% [x (- (send ori getX) (send size getX))] [y (- (send ori getY) (send size getY))] [z (- (send ori getZ) (send size getZ))]))
      (vector-set! currcoord 5 (new vec3% [x (+ (send ori getX) (send size getX))] [y (- (send ori getY) (send size getY))] [z (- (send ori getZ) (send size getZ))]))
      (vector-set! currcoord 6 (new vec3% [x (+ (send ori getX) (send size getX))] [y (- (send ori getY) (send size getY))] [z (+ (send ori getZ) (send size getZ))]))
      (vector-set! currcoord 7 (new vec3% [x (- (send ori getX) (send size getX))] [y (- (send ori getY) (send size getY))] [z (+ (send ori getZ) (send size getZ))])))
    ))

(define draw
  (lambda (myhead)
    (glbgnend GL_QUADS ; 0 1 2 3 上 蓝
              (glColor3f 0 0 1) 
              (glVertex3d (send (send myhead getCoords 0) getX) (send (send myhead getCoords 0) getY) (send (send myhead getCoords 0) getZ))
              (glVertex3d (send (send myhead getCoords 1) getX) (send (send myhead getCoords 1) getY) (send (send myhead getCoords 1) getZ))
              (glVertex3d (send (send myhead getCoords 2) getX) (send (send myhead getCoords 2) getY) (send (send myhead getCoords 2) getZ))
              (glVertex3d (send (send myhead getCoords 3) getX) (send (send myhead getCoords 3) getY) (send (send myhead getCoords 3) getZ)))
    (glbgnend GL_QUADS ; 7 6 2 3 前 黄
              (glColor3f 1 1 0) 
              (glVertex3d (send (send myhead getCoords 7) getX) (send (send myhead getCoords 7) getY) (send (send myhead getCoords 7) getZ))
              (glVertex3d (send (send myhead getCoords 6) getX) (send (send myhead getCoords 6) getY) (send (send myhead getCoords 6) getZ))
              (glVertex3d (send (send myhead getCoords 2) getX) (send (send myhead getCoords 2) getY) (send (send myhead getCoords 2) getZ))
              (glVertex3d (send (send myhead getCoords 3) getX) (send (send myhead getCoords 3) getY) (send (send myhead getCoords 3) getZ)))
    (glbgnend GL_QUADS ; 5 1 2 6 右 红
              (glColor3f 1 0 0)
              (glVertex3d (send (send myhead getCoords 5) getX) (send (send myhead getCoords 5) getY) (send (send myhead getCoords 5) getZ))
              (glVertex3d (send (send myhead getCoords 1) getX) (send (send myhead getCoords 1) getY) (send (send myhead getCoords 1) getZ))
              (glVertex3d (send (send myhead getCoords 2) getX) (send (send myhead getCoords 2) getY) (send (send myhead getCoords 2) getZ))
              (glVertex3d (send (send myhead getCoords 6) getX) (send (send myhead getCoords 6) getY) (send (send myhead getCoords 6) getZ)))
    (glbgnend GL_QUADS ; 4 5 6 7 下 蓝
              (glColor3f 0 0 1) 
              (glVertex3d (send (send myhead getCoords 4) getX) (send (send myhead getCoords 4) getY) (send (send myhead getCoords 4) getZ))
              (glVertex3d (send (send myhead getCoords 5) getX) (send (send myhead getCoords 5) getY) (send (send myhead getCoords 5) getZ))
              (glVertex3d (send (send myhead getCoords 6) getX) (send (send myhead getCoords 6) getY) (send (send myhead getCoords 6) getZ))
              (glVertex3d (send (send myhead getCoords 7) getX) (send (send myhead getCoords 7) getY) (send (send myhead getCoords 7) getZ)))
    (glbgnend GL_QUADS ; 4 0 3 7 左 红
              (glColor3f 1 0 0)
              (glVertex3d (send (send myhead getCoords 0) getX) (send (send myhead getCoords 0) getY) (send (send myhead getCoords 0) getZ))
              (glVertex3d (send (send myhead getCoords 4) getX) (send (send myhead getCoords 4) getY) (send (send myhead getCoords 4) getZ))
              (glVertex3d (send (send myhead getCoords 7) getX) (send (send myhead getCoords 7) getY) (send (send myhead getCoords 7) getZ))
              (glVertex3d (send (send myhead getCoords 3) getX) (send (send myhead getCoords 3) getY) (send (send myhead getCoords 3) getZ)))
    (glbgnend GL_QUADS ; 4 5 1 0 后 黄
              (glColor3f 1 1 0)
              (glVertex3d (send (send myhead getCoords 4) getX) (send (send myhead getCoords 4) getY) (send (send myhead getCoords 4) getZ))
              (glVertex3d (send (send myhead getCoords 5) getX) (send (send myhead getCoords 5) getY) (send (send myhead getCoords 5) getZ))
              (glVertex3d (send (send myhead getCoords 1) getX) (send (send myhead getCoords 1) getY) (send (send myhead getCoords 1) getZ))
              (glVertex3d (send (send myhead getCoords 0) getX) (send (send myhead getCoords 0) getY) (send (send myhead getCoords 0) getZ)))
    ))
  
(define (draw-opengl x y z vobj)
  (glClearColor 1 1 1 0.0)
  (glEnable GL_DEPTH_TEST)
  (glClear GL_COLOR_BUFFER_BIT)
  (glClear GL_DEPTH_BUFFER_BIT)
  (define max-axis (add1 (max x y z)))
  (glMatrixMode GL_PROJECTION)
  (glLoadIdentity)
  (glOrtho -10.0  10.0  -10.0  10.0  -10.0  10.0)
;  (glOrtho (/ (- max-axis) 2) max-axis (/ (- max-axis) 2) max-axis (/ (- max-axis) 2) max-axis)
;  (gluPerspective 30.0 0.75 0.6 20) ; 600 / 800

  (glMatrixMode GL_MODELVIEW)
  (glLoadIdentity)
  (glTranslatef 0.5 0.0 0.0)
  (glRotatef 5 1.0 0.0 0.0) ; 60
  (glRotatef 10 0.0 1.0 0.0) ; -60
  (glRotatef 5 0.0 0.0 1.0) ; 120
;  (for ([i vobj])
  ;    (draw i))
;  (draw head)
;  (draw myhead)
  (draw (vector-ref vobj 0)) ; head
  (draw (vector-ref vobj 1)) ; body

  (glPushMatrix)
  (glRotatef 45 1 0 0)
  (draw (vector-ref vobj 2)) ; larm
  (glPopMatrix)
  
  (draw (vector-ref vobj 3)) ; rarm
  (draw (vector-ref vobj 4)) ; lleg
  
  (glPushMatrix)
  (glRotatef -82 1 0 0)
  (draw (vector-ref vobj 5)) ; rleg
  (glPopMatrix)
  )

(define my-canvas%
  (class* canvas% ()
    (inherit with-gl-context swap-gl-buffers)
    (init-field (x 3) (y 4) (z 2) (vobj myv))
    (define/override (on-paint)
      (with-gl-context
        (lambda ()
          (draw-opengl x y z vobj)
          (swap-gl-buffers))))
    (define/override (on-size width height)
      (with-gl-context
        (lambda ()
          (resize width height))))
    (super-instantiate () (style '(gl)))
    ))

(define win (new frame% (label "Hello world, I am a Zombie!") (min-width 600) (min-height 800)))

(define head (new obj% [size (new vec3% [x 1] [y 1] [z 1])] [ori (new vec3% [x 0] [y 3.5] [z 0])] [ro (new vec3% [x 0] [y 1] [z 0])] [ax 3] [angle 3.54] [color "yellow"] [coord '()]))
(define hsize (new vec3% [x 1] [y 1] [z 1]))
(define hori (new vec3% [x 0] [y 3.5] [z 0]))
(send head updateCoords hsize hori)

(define body (new obj% [size (new vec3% [x 2] [y 2.5] [z 2])] [ori (new vec3% [x 0] [y 0] [z 0])] [ro (new vec3% [x 0] [y 2] [z 0])] [ax 2] [angle 2.55] [color "yellow"] [coord '()]))
(define bsize (new vec3% [x 2] [y 2.5] [z 2]))
(define bori (new vec3% [x 0] [y 0] [z 0]))
(send body updateCoords bsize bori)

(define larm (new obj% [size (new vec3% [x 0.5] [y 2.5] [z 0.5])] [ori (new vec3% [x -2.5] [y 0] [z 0])] [ro (new vec3% [x 0] [y 0.5] [z 0])] [ax 0.5] [angle 55] [color "yellow"] [coord '()]))
(define lasize (new vec3% [x 0.5] [y 2.5] [z 0.5]))
(define laori (new vec3% [x -2.5] [y 0] [z 0]))
(send larm updateCoords lasize laori)

(define rarm (new obj% [size (new vec3% [x 0.5] [y 2.5] [z 0.5])] [ori (new vec3% [x 2.5] [y 0] [z 0])] [ro (new vec3% [x 0] [y 0.5] [z 0])] [ax 0.5] [angle 55] [color "yellow"] [coord '()]))
(define rasize (new vec3% [x 0.5] [y 2.5] [z 0.5]))
(define raori (new vec3% [x 2.5] [y 0] [z 0]))
(send rarm updateCoords rasize raori)

(define lleg (new obj% [size (new vec3% [x 0.75] [y 3] [z 0.75])] [ori (new vec3% [x -0.75] [y -5.5] [z 0])] [ro (new vec3% [x 0] [y -5.5] [z 0])] [ax 0.75] [angle 55] [color "yellow"] [coord '()]))
(define llsize (new vec3% [x 0.75] [y 3] [z 0.75]))
(define llori (new vec3% [x -0.75] [y -5.5] [z 0]))
(send lleg updateCoords llsize llori)

(define rleg (new obj% [size (new vec3% [x 0.75] [y 3] [z 0.75])] [ori (new vec3% [x 0.75] [y -5.5] [z 0])] [ro (new vec3% [x 0] [y -5.5] [z 0])] [ax 0.75] [angle 55] [color "yerlow"] [coord '()]))
(define rlsize (new vec3% [x 0.75] [y 3] [z 0.75]))
(define rlori (new vec3% [x 0.75] [y -5.5] [z 0]))
(send rleg updateCoords rlsize rlori)

(define myv (vector head body larm rarm lleg rleg))

(define gl  (new my-canvas% (parent win) (x 6) (y 10) (z 4) (vobj myv)))
 
(send win show #t)
 

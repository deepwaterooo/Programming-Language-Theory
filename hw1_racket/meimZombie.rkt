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
    (define/public (set v) 
      (set! curr-x (send v getX))
      (set! curr-y (send v getY))
      (set! curr-z (send v getZ)))
    ))

(define obj%
  (class object%
    (init size ori ro ax angle color coord)
    (define currsize size)
    (define currori ori)
    (define currro ro)
    (define currax ax)
    (define currangle angle)
    (define currcolor color)
    (define currcoord coord)
    (super-new)
    (updateCoords)
    (define/public (get-size) currsize)
    (define/public (get-ori) currori)
    (define/public (get-ro) currro)
    (define/public (get-ax) currax)
    (define/public (get-angle) currangle)
    (define/public (get-color) currcolor)
    (define/public (getCoords idx)
      (vector-ref currcoord idx))
    (define/public (resetSize amt) 
      (send currsize set amt)
      (updateCoords))
    (define/public (resetOri ori)  
      (send currori set ori)
      (updateCoords))
    (define/public (resetRo ro)
      (send currro set ro))
    (define/public (resetAx ax)
      (set! currax ax))
    (define/public (resetAngle angle)
      (set! currangle angle))
    (define/public (resetColor color)
      (set! currcolor color))
    (define/public updateCoords
      (lambda ()
        (set! currcoord (make-vector 8))
        (vector-set! currcoord 0 (new vec3% [x (- (send currori getX) (send currsize getX))] [y (+ (send currori getY) (send currsize getY))] [z (- (send currori getZ) (send currsize getZ))]))
        (vector-set! currcoord 1 (new vec3% [x (+ (send currori getX) (send currsize getX))] [y (+ (send currori getY) (send currsize getY))] [z (- (send currori getZ) (send currsize getZ))]))
        (vector-set! currcoord 2 (new vec3% [x (+ (send currori getX) (send currsize getX))] [y (+ (send currori getY) (send currsize getY))] [z (+ (send currori getZ) (send currsize getZ))]))
        (vector-set! currcoord 3 (new vec3% [x (- (send currori getX) (send currsize getX))] [y (+ (send currori getY) (send currsize getY))] [z (+ (send currori getZ) (send currsize getZ))]))
        (vector-set! currcoord 4 (new vec3% [x (- (send currori getX) (send currsize getX))] [y (- (send currori getY) (send currsize getY))] [z (- (send currori getZ) (send currsize getZ))]))
        (vector-set! currcoord 5 (new vec3% [x (+ (send currori getX) (send currsize getX))] [y (- (send currori getY) (send currsize getY))] [z (- (send currori getZ) (send currsize getZ))]))
        (vector-set! currcoord 6 (new vec3% [x (+ (send currori getX) (send currsize getX))] [y (- (send currori getY) (send currsize getY))] [z (+ (send currori getZ) (send currsize getZ))]))
        (vector-set! currcoord 7 (new vec3% [x (- (send currori getX) (send currsize getX))] [y (- (send currori getY) (send currsize getY))] [z (+ (send currori getZ) (send currsize getZ))]))))
    (define/public trsX
      (lambda (dx)
        (glTranslatef dx 0 0)))
    (define/public trsY
      (lambda (dy)
        (glTranslatef 0 dy 0)))
    (define/public trsZ
      (lambda (dz)
        (glTranslatef 0 0 dz)))
    (define/public trs
      (lambda (dx dy dz)
        (glTranslatef dx 0 0)
        (glTranslatef 0 dy 0)
        (glTranslatef 0 0 dz)))
    (define/public trsXto0
      (lambda ()
        (glTranslatef (send currro getX) 0 0)))
    (define/public trsYto0
      (lambda ()
        (glTranslatef 0 (send currro getY) 0)))
    (define/public trsZto0
      (lambda ()
        (glTranslatef 0 0 (send currro getZ))))
    (define/public trsXtoRo
      (lambda ()
        (glTranslatef (- (send currro getX)) 0 0)))
    (define/public trsYtoRo
      (lambda ()
        (glTranslatef 0 (- (send currro getY)) 0)))
    (define/public trsZtoRo
      (lambda ()
        (glTranslatef 0 0 (- (send currro getZ)))))
    (define/public rotX
      (lambda (xangle)
        (glRotatef xangle 1 0 0)))
    (define/public rotY
      (lambda (yangle)
        (glRotatef yangle 0 1 0)))
    (define/public rotZ
      (lambda (zangle)
        (glRotatef zangle 0 0 1)))
    (define/public trsto0
      (lambda ()
        (glTranslatef (send currro getX) 0 0)
        (glTranslatef 0 (send currro getY) 0)
        (glTranslatef 0 0 (send currro getZ))))
    (define/public trstoRo
      (lambda ()
        (glTranslatef (- (send currro getX)) 0 0)
        (glTranslatef 0 (- (send currro getY)) 0)
        (glTranslatef 0 0 (- (send currro getZ)))))
    (define/public rot
      (lambda (xangle yangle zangle)
        (glRotatef xangle 1 0 0)
        (glRotatef yangle 0 1 0)
        (glRotatef zangle 0 0 1)))
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
(struct me (body head larm rarm lleg rleg leye reye) #:transparent #:mutable)

(define (draw-opengl zmb sec)
  (glClearColor 1 1 1 0.0)
  (glEnable GL_DEPTH_TEST)
  (glClear GL_COLOR_BUFFER_BIT)
  (glClear GL_DEPTH_BUFFER_BIT)
  (glMatrixMode GL_PROJECTION)
  (glLoadIdentity)
  (glOrtho -15.0  15.0  -10.0  10.0  -10.0  10.0)
;  (glOrtho (/ (- max-axis) 2) max-axis (/ (- max-axis) 2) max-axis (/ (- max-axis) 2) max-axis)
;  (gluPerspective 30.0 0.75 0.6 20) ; 600 / 800
  (glMatrixMode GL_MODELVIEW)
  (glLoadIdentity)
;  (glPushMatrix)      ; not self defined, difficult to control
;  (glTranslatef 0 1 0)
;  (glRotatef -60 0 0 1) ; rotate center wrong
;  (glTranslatef 0 3.8 0)
;  (gluSphere (gluNewQuadric) 1.0 15 15)  
;  (glTranslatef 0 -4.8 0)
;  (glPopMatrix)

  (glTranslatef 0.5 0.0 0.0)
  (glRotatef 5 1.0 0.0 0.0) ; 60
  (glRotatef 10 0.0 1.0 0.0) ; -60
  (glRotatef 5 0.0 0.0 1.0) ; 120

  (draw (me-body zmb))
  
  (glPushMatrix)
  (send (me-head zmb) rotY -60)
  (draw (me-head zmb)) 
  (glPushMatrix)
  (send (me-leye zmb) trsto0)
  (send (me-leye zmb) rotZ 45)
  (send (me-leye zmb) trstoRo)
  (draw (me-leye zmb))
  (glPopMatrix)
  (glPushMatrix)
  (send (me-reye zmb) trsto0)
  (send (me-reye zmb) rotZ -45)
  (send (me-reye zmb) trstoRo)
  (draw (me-reye zmb))
  (draw (me-reye zmb)) 
  (glPopMatrix)
  (glPopMatrix)
  
  (glPushMatrix)
  (send (me-larm zmb) trsYto0)
  (send (me-larm zmb) rotX 90)
  (send (me-larm zmb) trsYtoRo)
  (draw (me-larm zmb))
  (glPopMatrix)
  
  (glPushMatrix)
  (send (me-rarm zmb) trsYto0)
  (send (me-rarm zmb) rotX 60)
  (send (me-rarm zmb) trsYtoRo)
  (draw (me-rarm zmb))
  (glPopMatrix)
  
  (glPushMatrix)
  (send (me-lleg zmb) trsto0) 
  (send (me-lleg zmb) rotZ -45)
  (send (me-lleg zmb) trstoRo)
  (draw (me-lleg zmb))
  (glPopMatrix)
  
  (glPushMatrix)
  (send (me-rleg zmb) trsto0) 
  (send (me-rleg zmb) rotX -60)
  (send (me-rleg zmb) trstoRo)
  (draw (me-rleg zmb))
  (glPopMatrix)

  (glTranslatef 8 0 0)

  (draw (me-body sec))
  
  (glPushMatrix)
  (send (me-head sec) rotY -60)
  (draw (me-head sec)) 
  (glPushMatrix)
  (send (me-leye sec) trsto0)
  (send (me-leye sec) rotZ 45)
  (send (me-leye sec) trstoRo)
  (draw (me-leye sec))
  (glPopMatrix)
  (glPushMatrix)
  (send (me-reye sec) trsto0)
  (send (me-reye sec) rotZ -45)
  (send (me-reye sec) trstoRo)
  (draw (me-reye sec))
  (draw (me-reye sec)) 
  (glPopMatrix)
  (glPopMatrix)
  
  (glPushMatrix)
  (send (me-larm sec) trsYto0)
  (send (me-larm sec) rotX 90)
  (send (me-larm sec) trsYtoRo)
  (draw (me-larm sec))
  (glPopMatrix)
  
  (glPushMatrix)
  (send (me-rarm sec) trsYto0)
  (send (me-rarm sec) rotX 60)
  (send (me-rarm sec) trsYtoRo)
  (draw (me-rarm sec))
  (glPopMatrix)
  
  (glPushMatrix)
  (send (me-lleg sec) trsto0) 
  (send (me-lleg sec) rotZ -45)
  (send (me-lleg sec) trstoRo)
  (draw (me-lleg sec))
  (glPopMatrix)
  
  (glPushMatrix)
  (send (me-rleg sec) trsto0) 
  (send (me-rleg sec) rotX -60)
  (send (me-rleg sec) trstoRo)
  (draw (me-rleg sec))
  (glPopMatrix)
  )

(define timer
  (lambda (value refreshMills)
    (set! refreshMills 15)
;    gluPostRedisplay()      ;      // Post re-paint request to activate display()
;    gluTimerFunc(refreshMills timer 0)   ; // next timer call milliseconds later
    ))

(define my-canvas%
  (class* canvas% ()
    (inherit with-gl-context swap-gl-buffers)
    (init-field (me myv) (sec myv2))
    (define/override (on-paint)
      (with-gl-context
        (lambda ()
          (draw-opengl me sec)
;          (draw-opengl sec)
          (swap-gl-buffers))))
    (define/override (on-size width height)
      (with-gl-context
        (lambda ()
          (resize width height))))
    (super-instantiate () (style '(gl)))
    ))

(define win (new frame% (label "Hello world, I am a Zombie!") (min-width 600) (min-height 800)))

(define body (new obj% [size (new vec3% [x 2.0] [y 2.0] [z 1.0])] [ori (new vec3% [x 0.0] [y 0.0] [z 0.0])] [ro (new vec3% [x 0] [y 2] [z 0])] [ax 2] [angle 2.55] [color "yellow"] [coord '()]))
(define head (new obj% [size (new vec3% [x 0.9] [y 0.9] [z 0.9])] [ori (new vec3% [x 0.0] [y 2.9] [z 0.0])] [ro (new vec3% [x 0] [y 2.0] [z 0])] [ax 3] [angle 3.54] [color "yellow"] [coord '()]))
(define leye (new obj% [size (new vec3% [x 0.2] [y 0.2] [z 0.2])] [ori (new vec3% [x -0.4] [y 3.2] [z 0.9])] [ro (new vec3% [x -0.4] [y 3.2] [z 0.9])] [ax 3] [angle 3.54] [color "yellow"] [coord '()]))
(define reye (new obj% [size (new vec3% [x 0.1] [y 0.1] [z 0.1])] [ori (new vec3% [x 0.40] [y 3.2] [z 0.9])] [ro (new vec3% [x 0.40] [y 3.2] [z 0.9])] [ax 3] [angle 3.54] [color "yellow"] [coord '()]))
(define larm (new obj% [size (new vec3% [x 0.8] [y 2.0] [z 0.8])] [ori (new vec3% [x -2.8] [y 2.0] [z 0.0])] [ro (new vec3% [x -2.8] [y 2.0] [z 0])] [ax 0.8] [angle 55] [color "yellow"] [coord '()]))
(define rarm (new obj% [size (new vec3% [x 0.8] [y 2.0] [z 0.8])] [ori (new vec3% [x 2.80] [y 2.0] [z 0.0])] [ro (new vec3% [x 2.8] [y 2] [z 0])] [ax 0.8] [angle 55] [color "yellow"] [coord '()]))
(define lleg (new obj% [size (new vec3% [x 0.8] [y 3.5] [z 0.8])] [ori (new vec3% [x -0.8] [y -5.5] [z 0])] [ro (new vec3% [x -0.8] [y -2] [z 0])] [ax 0.8] [angle 55] [color "yellow"] [coord '()]))
(define rleg (new obj% [size (new vec3% [x 0.8] [y 3.5] [z 0.8])] [ori (new vec3% [x 0.8] [y -5.50] [z 0])] [ro (new vec3% [x 0.80] [y -2] [z 0])] [ax 0.8] [angle 55] [color "yerlow"] [coord '()]))

(define myv (me body head larm rarm lleg rleg leye reye))
(define myv2 (me body head larm rarm lleg rleg leye reye))
;(define gl (new my-canvas% (parent win) (me myv) myv2))
(define gl (new my-canvas% (parent win) (me myv) (sec myv2)))

(send win show #t)
 

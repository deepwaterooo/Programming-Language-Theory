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
    (define/public (set v) ; vec3%
      (set! curr-x (send v getX))
      (set! curr-y (send v getY))
      (set! curr-z (send v getZ)))
    ))

(define obj%
  (class object%
    (init size ori ro ax angle color coord) ; vec3%: size ori (x y z)
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
      (send currro set ro)
      )
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
        (vector-set! currcoord 7 (new vec3% [x (- (send currori getX) (send currsize getX))] [y (- (send currori getY) (send currsize getY))] [z (+ (send currori getZ) (send currsize getZ))])))
      )))

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

; (define translateToWorldCenter
;   (lambda (obj)
;     (glTranslatef (send (send obj get-ro) getX) 0 0)
;     (glTranslatef 0 (send (send obj get-ro) getY) 0)
;     (glTranslatef 0 0 (send (send obj get-ro) getZ))
;     ))
; (define translateToObjOri
;   (lambda (obj)
;     (glTranslatef (- (send (send obj get-ro) getX)) 0 0)
;     (glTranslatef 0 (- (send (send obj get-ro) getY)) 0) 
;     (glTranslatef 0 0 (- (send (send obj get-ro) getZ))) 
;     ))
  
  (glMatrixMode GL_MODELVIEW)
  (glLoadIdentity)
  (glTranslatef 0.5 0.0 0.0)
  (glRotatef 5 1.0 0.0 0.0) ; 60
  (glRotatef 10 0.0 1.0 0.0) ; -60
  (glRotatef 5 0.0 0.0 1.0) ; 120

  (draw (vector-ref vobj 0)) ; body
  
  (glPushMatrix)
;  (glRotatef 45 0 1 0)
  (draw (vector-ref vobj 1)) ; head
  (draw (vector-ref vobj 6)) ; leye
  (draw (vector-ref vobj 7)) ; reye
  (glPopMatrix)
  
  (glPushMatrix)
  (glTranslatef 0 (send (send (vector-ref vobj 2) get-ro) getY) 0)    ; trans y
  (glRotatef 45 1 0 0)
  (draw (vector-ref vobj 2)) ; larm
  (glTranslatef 0 (- (send (send (vector-ref vobj 2) get-ro) getY)) 0) 
  (glPopMatrix)

  
  (glPushMatrix)
  (glTranslatef 0 (send (send (vector-ref vobj 3) get-ro) getY) 0)    ; trans y
  (glRotatef 90 1 0 0)
  (draw (vector-ref vobj 3)) ; rarm
  (glTranslatef 0 (- (send (send (vector-ref vobj 3) get-ro) getY)) 0) 
  (glPopMatrix)
  
  (draw (vector-ref vobj 4)) ; lleg
  
  (glPushMatrix)
; x
  (glTranslatef 0 0 (send (send (vector-ref vobj 5) get-ro) getY))    ; trans z
  (glRotatef -60 1 0 0)
  (draw (vector-ref vobj 5)) ; rleg
  (glTranslatef 0 0 (- (send (send (vector-ref vobj 5) get-ro) getY))) 
  ; z
;  (glTranslatef (- (send (send (vector-ref vobj 5) get-ro) getX)) 0 0) 
;  (glTranslatef 0 0 (send (send (vector-ref vobj 5) get-ro) getY))    ; trans z
;  (glRotatef 30 0 0 1)
;  (draw (vector-ref vobj 5)) ; rleg
;  (glTranslatef (send (send (vector-ref vobj 5) get-ro) getX) 0 0)    ; trans z
;  (glTranslatef 0 0 (- (send (send (vector-ref vobj 5) get-ro) getY))) 
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

(define body (new obj% [size (new vec3% [x 2.0] [y 2.0] [z 1.0])] [ori (new vec3% [x 0.0] [y 0.0] [z 0.0])] [ro (new vec3% [x 0] [y 2] [z 0])] [ax 2] [angle 2.55] [color "yellow"] [coord '()]))
(define head (new obj% [size (new vec3% [x 0.9] [y 0.9] [z 0.9])] [ori (new vec3% [x 0.0] [y 2.9] [z 0.0])] [ro (new vec3% [x 0] [y 2.0] [z 0])] [ax 3] [angle 3.54] [color "yellow"] [coord '()]))

(define leye (new obj% [size (new vec3% [x 0.2] [y 0.2] [z 0.2])] [ori (new vec3% [x -0.4] [y 3.2] [z 0.9])] [ro (new vec3% [x -0.4] [y 3.2] [z 0.9])] [ax 3] [angle 3.54] [color "yellow"] [coord '()]))
(define reye (new obj% [size (new vec3% [x 0.1] [y 0.1] [z 0.1])] [ori (new vec3% [x 0.40] [y 3.2] [z 0.9])] [ro (new vec3% [x 0.40] [y 3.2] [z 0.9])] [ax 3] [angle 3.54] [color "yellow"] [coord '()]))

(define larm (new obj% [size (new vec3% [x 0.8] [y 2.0] [z 0.8])] [ori (new vec3% [x -2.8] [y 2.0] [z 0.0])] [ro (new vec3% [x -2.8] [y 2.0] [z 0])] [ax 0.8] [angle 55] [color "yellow"] [coord '()]))
(define rarm (new obj% [size (new vec3% [x 0.8] [y 2.0] [z 0.8])] [ori (new vec3% [x 2.8] [y 2.0] [z 0.0])] [ro (new vec3% [x 2.8] [y 2] [z 0])] [ax 0.8] [angle 55] [color "yellow"] [coord '()]))

(define lleg (new obj% [size (new vec3% [x 0.8] [y 3.5] [z 0.8])] [ori (new vec3% [x -0.8] [y -5.5] [z 0])] [ro (new vec3% [x -0.8] [y -2] [z 0])] [ax 0.8] [angle 55] [color "yellow"] [coord '()]))
(define rleg (new obj% [size (new vec3% [x 0.8] [y 3.5] [z 0.8])] [ori (new vec3% [x 0.8] [y -5.5] [z 0])] [ro (new vec3% [x 0.8] [y -2] [z 0])] [ax 0.8] [angle 55] [color "yerlow"] [coord '()]))

;(send head resetRo (new vec3% [x 0] [y 2.0] [z 0]))
;(send larm resetOri (new vec3% [x -2.8] [y 2.0] [z 0]))
;(send larm resetSize (new vec3% [x 2.8] [y 2.0] [z 0]))
;(send rarm resetRo (new vec3% [x 2.8] [y 2.0] [z 0]))
;(send lleg resetRo (new vec3% [x -0.85] [y -2.0] [z 0]))
;(send rleg resetRo (new vec3% [x 0.85] [y -2.0] [z 0]))
;(send head resetSize (new vec3% [x 0.8] [y 0.85] [z 0.9]))
;(send head resetOri (new vec3% [x 0] [y 2.85] [z 0]))
;(send lleg resetOri  (new vec3% [x -0.75] [y -5] [z 0]))
;(send rleg resetOri  (new vec3% [x 0.75] [y -5] [z 0]))

;                    0     1    2    3   4    5    6     7   
(define myv (vector body head larm rarm lleg rleg leye reye))

(define gl  (new my-canvas% (parent win) (x 6) (y 10) (z 8) (vobj myv)))
 
(send win show #t)
 

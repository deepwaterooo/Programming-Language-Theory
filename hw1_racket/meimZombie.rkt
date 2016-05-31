#! /usr/bin/env racket

#lang racket/gui
(require racket/class)
(require racket/gui/base)
(require sgl)
(require sgl/gl)
(require sgl/gl-vectors)

(define-syntax-rule (glbgnend Vertex-Mode statement ...)
  (let () (glBegin Vertex-Mode) statement ... (glEnd)))
(define-syntax-rule (glppm statement ...)
  (let () (glPushMatrix) statement ... (glPopMatrix)))
(define (resize w h) (glViewport 0 0 w h))

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
    (define/public (setX x) (set! curr-x x))
    (define/public (setY y) (set! curr-y y))
    (define/public (setZ z) (set! curr-z z))
    (define/public (set v) 
      (set! curr-x (send v getX))
      (set! curr-y (send v getY))
      (set! curr-z (send v getZ)))
    ))

(define obj%
  (class object%
    (init size ori ro ax angle color coord isStage?) ; remove ax
    (define currsize size)
    (define currori ori)
    (define currro ro)
    (define currax ax)
    (define currangle angle)
    (define currcolor color)
    (define currcoord coord)
    (define isS? isStage?)
    (super-new)
    (updateCoords)
    (define/public (get-size) currsize)
    (define/public (get-ori) currori)
    (define/public (get-ro) currro)
    (define/public (get-ax) currax)
    (define/public (get-angle) currangle)
    (define/public (get-color) currcolor)
    (define/public (get-isS) isS?)
    (define/public (resetSize amt) 
      (send currsize set amt)
      (updateCoords))
    (define/public (resetOri ori)  
      (send currori set ori)
      (updateCoords))
    (define/public (resetRo ro) (send currro set ro))
    (define/public (resetAx ax) (set! currax ax))
    (define/public (resetAngle angle)  (send currangle set angle))
    (define/public (resetAngleX xangle) (send currangle setX xangle))
    (define/public (resetAngleY yangle) (send currangle setY yangle))
    (define/public (resetAngleZ zangle) (send currangle setZ zangle))
    (define/public (resetColor color) (set! currcolor color))
    (define/public (reset-isS iss) (set! isS? iss))
    (define/public (getCoords idx) (vector-ref currcoord idx))
    (define/public updateCoords
      (lambda () (set! currcoord (make-vector 8))
        (vector-set! currcoord 0 (new vec3% [x (- (send currori getX) (send currsize getX))] [y (+ (send currori getY) (send currsize getY))] [z (- (send currori getZ) (send currsize getZ))]))
        (vector-set! currcoord 1 (new vec3% [x (+ (send currori getX) (send currsize getX))] [y (+ (send currori getY) (send currsize getY))] [z (- (send currori getZ) (send currsize getZ))]))
        (vector-set! currcoord 2 (new vec3% [x (+ (send currori getX) (send currsize getX))] [y (+ (send currori getY) (send currsize getY))] [z (+ (send currori getZ) (send currsize getZ))]))
        (vector-set! currcoord 3 (new vec3% [x (- (send currori getX) (send currsize getX))] [y (+ (send currori getY) (send currsize getY))] [z (+ (send currori getZ) (send currsize getZ))]))
        (vector-set! currcoord 4 (new vec3% [x (- (send currori getX) (send currsize getX))] [y (- (send currori getY) (send currsize getY))] [z (- (send currori getZ) (send currsize getZ))]))
        (vector-set! currcoord 5 (new vec3% [x (+ (send currori getX) (send currsize getX))] [y (- (send currori getY) (send currsize getY))] [z (- (send currori getZ) (send currsize getZ))]))
        (vector-set! currcoord 6 (new vec3% [x (+ (send currori getX) (send currsize getX))] [y (- (send currori getY) (send currsize getY))] [z (+ (send currori getZ) (send currsize getZ))]))
        (vector-set! currcoord 7 (new vec3% [x (- (send currori getX) (send currsize getX))] [y (- (send currori getY) (send currsize getY))] [z (+ (send currori getZ) (send currsize getZ))]))))
    (define/public trsX (lambda (dx) (glTranslatef dx 0 0)))
    (define/public trsY (lambda (dy) (glTranslatef 0 dy 0)))
    (define/public trsZ (lambda (dz) (glTranslatef 0 0 dz)))
    (define/public trs
      (lambda (dx dy dz)
        (glTranslatef dx 0 0)
        (glTranslatef 0 dy 0)
        (glTranslatef 0 0 dz)))
    (define/public trsXto0 (lambda () (glTranslatef (send currro getX) 0 0)))
    (define/public trsYto0 (lambda () (glTranslatef 0 (send currro getY) 0)))
    (define/public trsZto0 (lambda () (glTranslatef 0 0 (send currro getZ))))
    (define/public trsXtoRo (lambda () (glTranslatef (- (send currro getX)) 0 0)))
    (define/public trsYtoRo (lambda () (glTranslatef 0 (- (send currro getY)) 0)))
    (define/public trsZtoRo (lambda () (glTranslatef 0 0 (- (send currro getZ)))))
    (define/public rotX (lambda (xangle) (glRotatef xangle 1 0 0)))
    (define/public rotY (lambda (yangle) (glRotatef yangle 0 1 0)))
    (define/public rotZ (lambda (zangle) (glRotatef zangle 0 0 1)))
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
    (define/public rotExcute
      (lambda ()
        (glRotatef (send currangle getX) 1 0 0)
        (glRotatef (send currangle getY) 0 1 0)
        (glRotatef (send currangle getZ) 0 0 1)))
    ))

(define texture%
  (class object%
    (init [(initial-bitmap bitmap)])
    (field [width 0]
           [height 0]
           [textures #f])
    (define (get-width) width)
    (define (get-height) height)
    (define/public bind-texture
      (lambda () (when (not textures) (error 'bind-texture "Attempt to use disposed texture%"))
        (glBindTexture GL_TEXTURE_2D (gl-vector-ref textures 0))))
    (define (load-from-bitmap! bitmap)
      (when textures (dispose)) (set! textures (glGenTextures 1))
      (bind-texture)
      (define image-data
        (let ()
          (set! width (send bitmap get-width))
          (set! height (send bitmap get-height))
          (define dc (new bitmap-dc% [bitmap bitmap]))
          (define pixels (* width height))
          (define vec (make-gl-ubyte-vector (* pixels 4)))
          (define data (make-bytes (* pixels 4)))
          (send dc get-argb-pixels 0 0 width height data)
          (for ((i (in-range pixels)))
            (for ((j (in-range 4)))
              (gl-vector-set! vec (+ (* i 4) j) (bytes-ref data (+ (* i 4) (- 3 j))))))
          vec))
      (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
      (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
      (glTexImage2D GL_TEXTURE_2D 0 4 width height 0 GL_BGRA GL_UNSIGNED_BYTE image-data))
    (define dispose
      (when textures (glDeleteTextures textures) (set! textures #f)))
    (super-new)
    (load-from-bitmap! initial-bitmap)
    ))

(define *texture* (delay (new texture% [bitmap (make-object bitmap% "Screen_Shot_2016-05-27_at_6_08_17_PM.png" 'unknown #f)])))
  
(define my-canvas%
  (class* canvas% ()
    (inherit with-gl-context swap-gl-buffers)
    (init-field (me myv) (sec myv2) (stag stage))
    (define texture (delay (new texture% [bitmap (make-object bitmap% "Screen_Shot_2016-05-27_at_6_08_17_PM.png" 'unknown #f)])))
    (define init? #f)

    (define/override (on-paint)
      (with-gl-context
        (lambda ()
          (unless init?
            (glEnable GL_COLOR_MATERIAL)
            (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
            (glEnable GL_BLEND)
            (glEnable GL_TEXTURE_2D) ;; Standard Init
            (glShadeModel GL_SMOOTH)
            (glClearColor 0.9 0.9 0.9 1.0)
            (glClearDepth 1)
            (glEnable GL_DEPTH_TEST)
            (glDepthFunc GL_LEQUAL)
            (glHint GL_PERSPECTIVE_CORRECTION_HINT GL_NICEST)
;            (glEnable GL_LIGHT0) ;; default light
;            (glEnable GL_LIGHTING)
            (set! init? #t))
          
;          (define p (cc-superimpose (colorize (disk 100) "white") (text (format "~a" key-code) 'default 72)))
;          (send (force *texture*) load-from-bitmap!) ; this func call args is wrong
          ;          (pict->bitmap p) ;))  ;;;; don't need these

          (send (force texture) bind-texture)
;          (glBegin)
;          (glTexCoord2i 0 1)
;          (glVertex3d (send (send stag getCoords 0) getX) (send (send stag getCoords 0) getY) (send (send stag getCoords 0) getZ))
;          (glTexCoord2i 1 1)
;          (glVertex3d (send (send stag getCoords 1) getX) (send (send stag getCoords 1) getY) (send (send stag getCoords 1) getZ))
;          (glTexCoord2i 1 0)
;          (glVertex3d (send (send stag getCoords 2) getX) (send (send stag getCoords 2) getY) (send (send stag getCoords 2) getZ))
;          (glTexCoord2i 0 0)
;          (glVertex3d (send (send stag getCoords 3) getX) (send (send stag getCoords 3) getY) (send (send stag getCoords 3) getZ))
;          (glEnd)
          (send (force texture) bind-texture)
          (draw-opengl me sec stag)
          (swap-gl-buffers))
        ))
    (define/override (on-size width height)
      (with-gl-context
        (lambda ()
          (resize width height))))
    (super-instantiate () (style '(gl)))))
(define win (new frame% (label "Hello world, We Are Zombies~!") (min-width 600) (min-height 800)))

(define draw
  (lambda (myhead)
    (glbgnend GL_QUADS (glColor3f 0 0 1) ; 0 1 2 3 上 蓝
;              (cond [(equal? (send myhead get-isS) #t) (lambda ()
                                               (send (force *texture*) bind-texture)
              (glTexCoord2i 0 1)
              (glVertex3d (send (send myhead getCoords 0) getX) (send (send myhead getCoords 0) getY) (send (send myhead getCoords 0) getZ))
              (glTexCoord2i 1 1)
              (glVertex3d (send (send myhead getCoords 1) getX) (send (send myhead getCoords 1) getY) (send (send myhead getCoords 1) getZ))
              (glTexCoord2i 1 0)
              (glVertex3d (send (send myhead getCoords 2) getX) (send (send myhead getCoords 2) getY) (send (send myhead getCoords 2) getZ))
              (glTexCoord2i 0 0)
              (glVertex3d (send (send myhead getCoords 3) getX) (send (send myhead getCoords 3) getY) (send (send myhead getCoords 3) getZ)))
;                    [else ( lambda () 
;                           (glVertex3d (send (send myhead getCoords 0) getX) (send (send myhead getCoords 0) getY) (send (send myhead getCoords 0) getZ))
;                           (glVertex3d (send (send myhead getCoords 1) getX) (send (send myhead getCoords 1) getY) (send (send myhead getCoords 1) getZ))
;                           (glVertex3d (send (send myhead getCoords 2) getX) (send (send myhead getCoords 2) getY) (send (send myhead getCoords 2) getZ))
;                           (glVertex3d (send (send myhead getCoords 3) getX) (send (send myhead getCoords 3) getY) (send (send myhead getCoords 3) getZ)))
                          
    (glbgnend GL_QUADS (glColor3f 1 1 0) ; 7 6 2 3 前 黄
              (glTexCoord2i 0 1)
              (glVertex3d (send (send myhead getCoords 7) getX) (send (send myhead getCoords 7) getY) (send (send myhead getCoords 7) getZ))
              (glTexCoord2i 1 1)
              (glVertex3d (send (send myhead getCoords 6) getX) (send (send myhead getCoords 6) getY) (send (send myhead getCoords 6) getZ))
              (glTexCoord2i 1 0)
              (glVertex3d (send (send myhead getCoords 2) getX) (send (send myhead getCoords 2) getY) (send (send myhead getCoords 2) getZ))
              (glTexCoord2i 0 0)
              (glVertex3d (send (send myhead getCoords 3) getX) (send (send myhead getCoords 3) getY) (send (send myhead getCoords 3) getZ)))
    (glbgnend GL_QUADS (glColor3f 1 0 0) ; 5 1 2 6 右 红
              (glTexCoord2i 0 1)
              (glVertex3d (send (send myhead getCoords 5) getX) (send (send myhead getCoords 5) getY) (send (send myhead getCoords 5) getZ))
              (glTexCoord2i 1 1)
              (glVertex3d (send (send myhead getCoords 1) getX) (send (send myhead getCoords 1) getY) (send (send myhead getCoords 1) getZ))
              (glTexCoord2i 1 0)
              (glVertex3d (send (send myhead getCoords 2) getX) (send (send myhead getCoords 2) getY) (send (send myhead getCoords 2) getZ))
              (glTexCoord2i 0 0)
              (glVertex3d (send (send myhead getCoords 6) getX) (send (send myhead getCoords 6) getY) (send (send myhead getCoords 6) getZ)))
    (glbgnend GL_QUADS (glColor3f 0 0 1) ; 4 5 6 7 下 蓝
              (glTexCoord2i 0 1)
              (glVertex3d (send (send myhead getCoords 4) getX) (send (send myhead getCoords 4) getY) (send (send myhead getCoords 4) getZ))
              (glTexCoord2i 1 1)
              (glVertex3d (send (send myhead getCoords 5) getX) (send (send myhead getCoords 5) getY) (send (send myhead getCoords 5) getZ))
              (glTexCoord2i 1 0)
              (glVertex3d (send (send myhead getCoords 6) getX) (send (send myhead getCoords 6) getY) (send (send myhead getCoords 6) getZ))
              (glTexCoord2i 0 0)
              (glVertex3d (send (send myhead getCoords 7) getX) (send (send myhead getCoords 7) getY) (send (send myhead getCoords 7) getZ)))
    (glbgnend GL_QUADS (glColor3f 1 0 0) ; 4 0 3 7 左 红
              (glTexCoord2i 0 1)
              (glVertex3d (send (send myhead getCoords 0) getX) (send (send myhead getCoords 0) getY) (send (send myhead getCoords 0) getZ))
              (glTexCoord2i 1 1)
              (glVertex3d (send (send myhead getCoords 4) getX) (send (send myhead getCoords 4) getY) (send (send myhead getCoords 4) getZ))
              (glTexCoord2i 1 0)
              (glVertex3d (send (send myhead getCoords 7) getX) (send (send myhead getCoords 7) getY) (send (send myhead getCoords 7) getZ))
              (glTexCoord2i 0 0)
              (glVertex3d (send (send myhead getCoords 3) getX) (send (send myhead getCoords 3) getY) (send (send myhead getCoords 3) getZ)))
    (glbgnend GL_QUADS (glColor3f 1 1 0) ; 4 5 1 0 后 黄
              (glTexCoord2i 0 1)
              (glVertex3d (send (send myhead getCoords 4) getX) (send (send myhead getCoords 4) getY) (send (send myhead getCoords 4) getZ))
              (glTexCoord2i 1 1)
              (glVertex3d (send (send myhead getCoords 5) getX) (send (send myhead getCoords 5) getY) (send (send myhead getCoords 5) getZ))
              (glTexCoord2i 1 0)
              (glVertex3d (send (send myhead getCoords 1) getX) (send (send myhead getCoords 1) getY) (send (send myhead getCoords 1) getZ))
              (glTexCoord2i 0 0)
              (glVertex3d (send (send myhead getCoords 0) getX) (send (send myhead getCoords 0) getY) (send (send myhead getCoords 0) getZ)))))

(define (draw-opengl zmb sec stag) ; vector
  (glClearColor 1 1 1 0.0)
  (glEnable GL_DEPTH_TEST)
  (glClear GL_COLOR_BUFFER_BIT)
  (glClear GL_DEPTH_BUFFER_BIT)

  (glEnable GL_COLOR_MATERIAL)
  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  (glEnable GL_BLEND)
  (glEnable GL_TEXTURE_2D) ;; Standard Init
  (glShadeModel GL_SMOOTH)
  (glClearColor 0.9 0.9 0.9 1.0)
  (glClearDepth 1)
  (glEnable GL_DEPTH_TEST)
  (glDepthFunc GL_LEQUAL)
  (glHint GL_PERSPECTIVE_CORRECTION_HINT GL_NICEST)
  
  (glMatrixMode GL_PROJECTION)
  (glLoadIdentity)
  (glOrtho -25.0  25.0  -25.0  25.0  -25.0  25.0)
;  (glOrtho (/ (- max-axis) 2) max-axis (/ (- max-axis) 2) max-axis (/ (- max-axis) 2) max-axis)
;  (gluPerspective 30.0 0.75 0.6 20) ; 600 / 800
  (glMatrixMode GL_MODELVIEW)
  (glLoadIdentity)
;  (glPushMatrix)      ; not self defined, difficult to control
;  (glTranslatef 0 4.8 0)
;  (glRotatef -90 0 0 1) ; rotate center wrong
;  (glTranslatef 0 -4.8 0)
;  (gluSphere (gluNewQuadric) 1.0 15 15)  
;  (glPopMatrix)

  (glTranslatef -5 0.0 0.0)
  (glRotatef 5 1.0 0.0 0.0) ; 60
  (glRotatef 10 0.0 1.0 0.0) ; -60
  (glRotatef 5 0.0 0.0 1.0) ; 120
  
; (glppm
;  (glRotatef 90 1 0 0)

  (send (force *texture*) bind-texture)
;  (glbgnend GL_QUADS (glColor3f 1 1 1) ; 9
;   (glTexCoord2i 0 1)
;   (glVertex3d -25 -9.4 -25)
;   (glTexCoord2i 1 1)
;   (glVertex3d 25 -9.4 -25)
;   (glTexCoord2i 1 0)
;   (glVertex3d 25 -9.4 25)
;   (glTexCoord2i 0 0)
;   (glVertex3d -25 -9.4 25))
  (draw stag)
  
  (drawZmbs zmb)
;  (draw (me-body zmb))
;  (drawHead (me-head zmb))
;  (drawArms (me-larm zmb))
;  (glppm ; rarm
;   (send (me-rarm zmb) trsYto0)
;   (send (me-rarm zmb) rotX 60)
;   (send (me-rarm zmb) trsYtoRo)
;   (draw (me-rarm zmb)))
;  (glppm ; lleg
;   (send (me-lleg zmb) trsto0) 
;   (send (me-lleg zmb) rotZ -45)
;   (send (me-lleg zmb) trstoRo)
;   (draw (me-lleg zmb)))
;  (drawLegs (me-rleg zmb))
;  ) ; (glppm) ; ck

;  (glTranslatef 8 0 0) ;; 31
;
;  ; serve as control
;  (draw (me-body sec)) 
;
;  (send (head-hd (me-head sec)) resetAngle (new vec3% [x 0] [y -50] [z 0]))
;  (send (head-le (me-head sec)) resetAngle (new vec3% [x 0] [y 0] [z 45]))
;  (send (head-re (me-head sec)) resetAngle (new vec3% [x 0] [y 0] [z -45]))
;  (send (head-mo (me-head sec)) resetAngle (new vec3% [x 0] [y 0] [z 45]))
;  (drawHead (me-head sec))
;
;  (send (arms-la (me-larm sec)) resetAngle (new vec3% [x 0] [y 0] [z 0]))
;  (send (arms-sa (me-larm sec)) resetAngle (new vec3% [x 0] [y 0] [z 0]))
;  (send (arms-fg (me-larm sec)) resetAngle (new vec3% [x 0] [y 0] [z 0]))
;  (drawArms (me-larm sec))
;
;  (glppm
;   (send (me-rarm sec) trsYto0)
;   (send (me-rarm sec) rotX 60)
;   (send (me-rarm sec) trsYtoRo)
;   (draw (me-rarm sec)))
;  (glppm
;   (send (me-lleg sec) trsto0) 
;   (send (me-lleg sec) rotZ -45)
;   (send (me-lleg sec) trstoRo)
;   (draw (me-lleg sec)))
;
;  (send (legs-ll (me-rleg sec)) resetAngle (new vec3% [x -90] [y 0] [z 60]))
;  (send (legs-sl (me-rleg sec)) resetAngle (new vec3% [x 135] [y 0] [z 0]))
;  (send (legs-ft (me-rleg sec)) resetAngle (new vec3% [x 90] [y 0] [z 0]))
;  (drawLegs (me-rleg sec))
  )

(define timer
  (lambda (value refreshMills)
    (set! refreshMills 15)
;    gluPostRedisplay()      ;      // Post re-paint request to activate display()
;    gluTimerFunc(refreshMills timer 0)   ; // next timer call milliseconds later
    ))

(define body (new obj% [size (new vec3% [x 2.0] [y 2.0] [z 1.0])] [ori (new vec3% [x 0.00] [y 0.0] [z 0.0])] [ro (new vec3% [x 0.0] [y 2.0] [z 0.0])] [ax 2.0]  [angle (new vec3% [x 0] [y 0] [z 0])] [color "yellow"] [coord '()] [isStage? #f]))
(struct head (hd le re mo))
(define hbox (new obj% [size (new vec3% [x 0.9] [y 0.9] [z 0.9])] [ori (new vec3% [x 0.00] [y 2.9] [z 0.0])] [ro (new vec3% [x 0.00] [y 2.0] [z 0.0])] [ax 3.0] [angle (new vec3% [x 0] [y 0] [z 0])] [color "yellow"] [coord '()] [isStage? #f]))
(define leye (new obj% [size (new vec3% [x 0.2] [y 0.2] [z 0.3])] [ori (new vec3% [x -0.4] [y 3.2] [z 0.9])] [ro (new vec3% [x -0.4] [y 3.2] [z 0.9])] [ax 3.0] [angle (new vec3% [x 0] [y 0] [z 45])] [color "yellow"] [coord '()] [isStage? #f]))
(define reye (new obj% [size (new vec3% [x 0.1] [y 0.1] [z 0.2])] [ori (new vec3% [x 0.40] [y 3.2] [z 0.9])] [ro (new vec3% [x 0.40] [y 3.2] [z 0.9])] [ax 3.0] [angle (new vec3% [x 0] [y 0] [z -45])] [color "yellow"] [coord '()] [isStage? #f]))
(define moth (new obj% [size (new vec3% [x 0.3] [y 0.2] [z 0.3])] [ori (new vec3% [x 0.00] [y 2.5] [z 0.9])] [ro (new vec3% [x 0.00] [y 2.5] [z 0.9])] [ax 3.0] [angle (new vec3% [x 0] [y 0] [z 45])] [color "yellow"] [coord '()] [isStage? #f]))
(define myhead (head hbox leye reye moth))
(define hbox2 (new obj% [size (new vec3% [x 0.9] [y 0.9] [z 0.9])] [ori (new vec3% [x 0.00] [y 2.9] [z 0.0])] [ro (new vec3% [x 0.00] [y 2.0] [z 0.0])] [ax 3.0] [angle (new vec3% [x 0] [y 0] [z 0])] [color "yellow"] [coord '()] [isStage? #f]))
(define myhead2 (head hbox2 leye reye moth))
(define (drawHead head) ; head : { hd, then le | re | mo separately }
  (glppm 
   (send (head-hd head) trsto0)
   (send (head-hd head) rotExcute)
   (send (head-hd head) trstoRo)
   (draw (head-hd head))
   (glppm
    (send (head-le head) trsto0)
    (send (head-le head) rotExcute)
    (send (head-le head) trstoRo)
    (draw (head-le head)))
   (glppm
    (send (head-re head) trsto0)
    (send (head-re head) rotExcute)
    (send (head-re head) trstoRo)
    (draw (head-re head)))
   (glppm
    (send (head-mo head) trsto0)
    (send (head-mo head) rotExcute)
    (send (head-mo head) trstoRo)
    (draw (head-mo head)))))

(struct arms (la sa fg))
(define mylla (new obj% [size (new vec3% [x 0.8] [y 1.0] [z 0.8])] [ori (new vec3% [x -2.8] [y 1.00] [z 0.0])] [ro (new vec3% [x -2.8] [y 2.0] [z 0.0])] [ax 0.8] [angle (new vec3% [x -90] [y -45] [z 0])]  [color "yellow"] [coord '()] [isStage? #f]))
(define mylsa (new obj% [size (new vec3% [x 0.5] [y 0.8] [z 0.5])] [ori (new vec3% [x -2.8] [y -0.8] [z 0.0])] [ro (new vec3% [x -2.8] [y 0.0] [z 0.0])] [ax 0.8] [angle (new vec3% [x -90] [y 0] [z 90])]  [color "yellow"] [coord '()] [isStage? #f]))
(define mylfg (new obj% [size (new vec3% [x 0.1] [y 0.4] [z 0.3])] [ori (new vec3% [x -2.8] [y -2.0] [z 0.0])] [ro (new vec3% [x -2.8] [y -1.6] [z 0.0])] [ax 0.8] [angle (new vec3% [x 0] [y 0] [z 45])]  [color "yellow"] [coord '()] [isStage? #f]))
(define larm (arms mylla mylsa mylfg))
(define mylla2 (new obj% [size (new vec3% [x 0.8] [y 1.0] [z 0.8])] [ori (new vec3% [x -2.8] [y 1.00] [z 0.0])] [ro (new vec3% [x -2.8] [y 2.0] [z 0.0])] [ax 0.8] [angle (new vec3% [x -90] [y -45] [z 0])]  [color "yellow"] [coord '()] [isStage? #f]))
(define mylsa2 (new obj% [size (new vec3% [x 0.5] [y 0.8] [z 0.5])] [ori (new vec3% [x -2.8] [y -0.8] [z 0.0])] [ro (new vec3% [x -2.8] [y 0.0] [z 0.0])] [ax 0.8] [angle (new vec3% [x -90] [y 0] [z 90])]  [color "yellow"] [coord '()] [isStage? #f]))
(define mylfg2 (new obj% [size (new vec3% [x 0.1] [y 0.4] [z 0.3])] [ori (new vec3% [x -2.8] [y -2.0] [z 0.0])] [ro (new vec3% [x -2.8] [y -1.6] [z 0.0])] [ax 0.8] [angle (new vec3% [x 0] [y 0] [z 45])]  [color "yellow"] [coord '()] [isStage? #f]))
(define larm2 (arms mylla2 mylsa2 mylfg2))
(define rarm (new obj% [size (new vec3% [x 0.8] [y 2.0] [z 0.8])] [ori (new vec3% [x 2.80] [y 2.0] [z 0.0])] [ro (new vec3% [x 2.80] [y 2.0] [z 0.0])] [ax 0.8] [angle (new vec3% [x 0] [y 0] [z 0])]  [color "yellow"] [coord '()] [isStage? #f]))
(define (drawArms arms) ; arms : { la { sa { fg } } }
  (glppm
   (send (arms-la arms) trsto0)
   (send (arms-la arms) rotExcute)
   (send (arms-la arms) trstoRo)
   (draw (arms-la arms))
   (glppm
    (send (arms-sa arms) trsto0)
    (send (arms-sa arms) rotExcute)
    (send (arms-sa arms) trstoRo)
    (draw (arms-sa arms))
    (glppm
     (send (arms-fg arms) trsto0)
     (send (arms-fg arms) rotExcute)
     (send (arms-fg arms) trstoRo)
     (draw (arms-fg arms)))
    )))

(struct legs (ll sl ft))
(define myrll (new obj% [size (new vec3% [x 0.8] [y 2.0] [z 0.8])] [ori (new vec3% [x 0.80] [y -4.0] [z 0])]  [ro (new vec3% [x 0.80] [y -2.00] [z 0.0])] [ax 0.8] [angle (new vec3% [x 0] [y 0] [z 0])] [color "yerlow"] [coord '()] [isStage? #f]))
(define myrsl (new obj% [size (new vec3% [x 0.5] [y 1.5] [z 0.5])] [ori (new vec3% [x 0.80] [y -7.5] [z 0.0])] [ro (new vec3% [x 0.80] [y -6.0] [z 0.0])] [ax 0.8] [angle (new vec3% [x 0] [y 0] [z 0])]  [color "yellow"] [coord '()] [isStage? #f]))
(define myrft (new obj% [size (new vec3% [x 0.6] [y 0.2] [z 0.7])] [ori (new vec3% [x 0.80] [y -9.2] [z 0.4])] [ro (new vec3% [x 0.80] [y -9.0] [z 0.0])] [ax 0.8] [angle (new vec3% [x 0] [y 0] [z 0])]  [color "yellow"] [coord '()] [isStage? #f]))
(define rleg (legs myrll myrsl myrft))
(define myrll2 (new obj% [size (new vec3% [x 0.8] [y 2.0] [z 0.8])] [ori (new vec3% [x 0.80] [y -4.0] [z 0])]  [ro (new vec3% [x 0.80] [y -2.00] [z 0.0])] [ax 0.8] [angle (new vec3% [x 0] [y 0] [z 0])] [color "yerlow"] [coord '()] [isStage? #f]))
(define myrsl2 (new obj% [size (new vec3% [x 0.5] [y 1.5] [z 0.5])] [ori (new vec3% [x 0.80] [y -7.5] [z 0.0])] [ro (new vec3% [x 0.80] [y -6.0] [z 0.0])] [ax 0.8] [angle (new vec3% [x 0] [y 0] [z 0])]  [color "yellow"] [coord '()] [isStage? #f]))
(define myrft2 (new obj% [size (new vec3% [x 0.6] [y 0.2] [z 0.7])] [ori (new vec3% [x 0.80] [y -9.2] [z 0.4])] [ro (new vec3% [x 0.80] [y -9.0] [z 0.0])] [ax 0.8] [angle (new vec3% [x 0] [y 0] [z 0])]  [color "yellow"] [coord '()] [isStage? #f]))
(define rleg2 (legs myrll2 myrsl2 myrft2))
(define lleg (new obj% [size (new vec3% [x 0.8] [y 3.5] [z 0.8])] [ori (new vec3% [x -0.8] [y -5.5] [z 0])]  [ro (new vec3% [x -0.8] [y -2.0] [z 0.0])] [ax 0.8] [angle (new vec3% [x 0] [y 0] [z 0])] [color "yellow"] [coord '()] [isStage? #f]))
(define (drawLegs legs) ; legs : { ll { sl { ft } } }
  (glppm
   (send (legs-ll legs) trsto0)
   (send (legs-ll legs) rotExcute)
   (send (legs-ll legs) trstoRo)
   (draw (legs-ll legs))
   (glppm
    (send (legs-sl legs) trsto0)
    (send (legs-sl legs) rotExcute)
    (send (legs-sl legs) trstoRo)
    (draw (legs-sl legs))
    (glppm
     (send (legs-ft legs) trsto0)
     (send (legs-ft legs) rotExcute)
     (send (legs-ft legs) trstoRo)
     (draw (legs-ft legs)))
    )))

(struct me (body head larm rarm lleg rleg) #:transparent #:mutable)
(define (drawZmbs zmb) ; draw one zombie, later on loop if vector of zmbs
  (glppm
   (send (me-body zmb) trsto0)
   (send (me-body zmb) rotExcute)
   (send (me-body zmb) trstoRo)
   (draw (me-body zmb)) ; still need generic this one
   (drawHead (me-head zmb))
   (drawArms (me-larm zmb))
;   (drawArms (me-rarm zmb))
;   (drawLegs (me-lleg zmb))
  (glppm ; rarm, fix these later
   (send (me-rarm zmb) trsYto0)
   (send (me-rarm zmb) rotX 60)
   (send (me-rarm zmb) trsYtoRo)
   (draw (me-rarm zmb)))
  (glppm ; lleg
   (send (me-lleg zmb) trsto0) 
   (send (me-lleg zmb) rotZ -45)
   (send (me-lleg zmb) trstoRo)
   (draw (me-lleg zmb)))
  
   (drawLegs (me-rleg zmb))
   ))

(define stage (new obj% [size (new vec3% [x 25.0] [y 0.5] [z 25.0])] [ori (new vec3% [x 0.00] [y -9.9] [z 0.0])] [ro (new vec3% [x 0.0] [y -9.4] [z 0.0])] [ax 2.0]  [angle (new vec3% [x 0] [y 0] [z 0])] [color "yellow"] [coord '()] [isStage? #t]))

(define myv (me body myhead larm rarm lleg rleg))
(define myv2 (struct-copy me myv [head myhead2] [larm larm2] [rleg rleg2]))

(define gl (new my-canvas% (parent win) (me myv) (sec myv2) (stag stage)))

(send win show #t)

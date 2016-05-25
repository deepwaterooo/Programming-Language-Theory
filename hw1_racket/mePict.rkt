#! /usr/bin/env racket

#lang racket
(require pict3d)
(require pict3d/universe)

(define obj%
  (class object%
    (init size ori ro ax angle color)
    (super-new)
    (define current-size size)
    (define current-ori ori)
    (define current-ro ro)
    (define current-ax ax)
    (define current-angle angle)
    (define current-color color)
    (define/public (get-size) current-size)
    (define/public (get-ori) current-ori)
    (define/public (get-ro) current-ro)
    (define/public (get-ax) current-ax)
    (define/public (get-angle) current-angle)
    (define/public (get-color) current-color)
;    (define/public draw (print current-size))
    (define/public (grow amt)
      (set! current-size (+ current-size amt)))
    (define/public (resetOri ori)
      (set! current-ori ori))
    (define/public (resetRo ro)
      (set! current-ro ro))
    (define/public (resetAx ax)
      (set! current-ax ax))
    (define/public (resetAngle angle)
      (set! current-angle angle))
    (define/public (resetColor color)
      (set! current-color color))
    ))

(define myhead (new obj% [size 2] [ori (pos 0 -5 0)] [ro (pos 0 3 0)] [ax 3] [angle 45] [color (rgba "blue" 0)]))

(define (draw obj)
  (sphere
   (send obj get-ori)
   (send obj get-size)))

(current-material (material #:ambient 0.01
                            #:diffuse 0.39
                            #:specular 0.6
                            #:roughness 0.2))
(define lights+camera
  (combine (light (pos 0 1 2) (emitted "Thistle"))
           (light (pos 0 -1 -2) (emitted "PowderBlue"))
           (basis 'camera (point-at (pos 0 0 15) origin)))) ; 1 1 0
 
(define (on-draw s n t)
  (combine
   ;(rotate-z (rotate-y (rotate-x
   (rotate-y
     (draw myhead)
    (/ t 10)
;                                (/ t 11))
;                               (/ t 13))
    ;                     (/ t 17))
    )
;    (cube (pos 0 -3 0) 3)
     (cube origin 3)
    lights+camera))
  
(big-bang3d 0 #:on-draw on-draw)


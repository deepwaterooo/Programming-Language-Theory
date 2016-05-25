#! /usr/bin/env racket

#lang racket
;(require racket/gui)

(require 2htdp/image
         (only-in racket/gui/base play-sound))


;(define frame (new frame% [label "Zombie Dancing"]
;                   [width 1000]
;                   [height 800]))
;(define msg (new message% [parent frame]
;                 [label "No events so far yet..."]))

(define frame (empty-scene 700 500))

(define x 0)


;;; a house: victorian
(define house 
  (above (beside/align "bottom"
                       (triangle 200 "solid" "red")
                       (triangle 150 "solid" "red"))
         (rectangle 350 250 "solid" "blue")))
(define door (rectangle 100 180 "solid" "brown"))
(define door-with-know
  (overlay/align "right" "center" (circle 12 "solid" "yellow") door))
(define victorian
  (overlay/align "center" "bottom" door-with-know house))


;;; adapt to a zombie ~ me
; head
(define head (underlay/offset (ellipse 50 100 "solid" "gray")
                              0 -2
                              (underlay/offset (rectangle 20 10 'solid "brown")
                                               0 -38
                                               (underlay/offset (rectangle 20 20 'solid "red")
                                                                -25 0
                                                                (circle 10 "solid" "blue")))))
; body
(define body
  (overlay/offset (rectangle 45 10 "solid" "red")
                  20 10
                  (overlay/offset (rectangle 55 90 "solid" "brown") 30 -5 (rectangle 45 10 "solid" "yellow"))))
; legs
(define legleft (triangle/aas 110 8 150 "solid" "lightseagreen"))
(define legright (triangle/aas 110 8 150 "solid" "aquamarine"))
;(define legleft (rectangle 12 120 "solid" "lightseagreen"))
;(define legright (rectangle 12 120 "solid" "aquamarine"))

; zombie
(define zombie 
  (overlay/offset head 60 102
                  (overlay/offset  body 50 50
                                   (overlay/offset legleft -20 12 legright))))
(define fun
  (overlay/offset victorian 300 100 
                  zombie))


;(play-sound "sounds/ilovemyfamily.mp4" #t)
(play-sound "sounds/e2.mp3" #t)

(define (my-rotate angle)
  (overlay/offset victorian 300 100 
                  (rotate angle zombie)))

(my-rotate 10)
(place-image fun 290 280 frame)

(my-rotate 20)
(place-image fun 300 380 frame)

(my-rotate 40)
(place-image fun 300 380 frame)

(my-rotate 60)
(place-image fun 300 380 frame)

(my-rotate 80)
(place-image fun 300 380 frame)

(my-rotate 100)
(place-image fun 300 380 frame)


(define (loop)
;  (send canvas on-paint)
  (place-image fun 300 380 frame)
  (set! x (+ x 10))
  (my-rotate x)
;  (sleep/yield 0.01)  ; seems here don't work
  (loop))

;(loop)



;(require racket/class)
; Derive a new canvas (a drawing window) class to handle events
;(define my-canvas%
;  (class canvas%   ; The base class is canvas%
;    ; Define overriding method to handle mouse events
;    (define/override (on-event event)
;      (send msg set-label "Canvas mouse"))
;    ; Define overriding method to handle keyboard events
;    (define/override (on-char event)
;      (send msg set-label "Canvas keyboard"))
;    ; Call the superclass init, passing on all init args
;    (super-new)))
;
;; Make a canvas that handles events in the frame
;(define canvas (new my-canvas% [parent frame]
;     [paint-callback
;      (lambda (canvas dc)
;        (send dc set-scale 3 3)
;        (send dc set-text-foreground "blue")
;        (send dc set-pen "yellow" 4 'solid)
;        (send dc clear)
;        (send dc draw-line x 250 x 250)
;        ;        (send dc draw-text "Don't Panic!" 0 0)
;        )]))
;
;(define panel (new horizontal-panel% [parent frame]
;                   [alignment '(center center)]))
;(new button% [parent panel]
;     [label "Start"]
;     [callback (lambda (button event)
;                 (send msg set-label "Button click"))])
;(new button% [parent panel]
;     [label "Pause"]
;     [callback (lambda (button event)
;                 (send msg set-label "Pause Button click")
;                 (sleep 3))])
;
;(define (loop)
;  (send canvas on-paint)
;  (set! x (remainder (+ x 1) 500))
;  (sleep/yield 0.01)  ; seems here don't work
;  (loop))
;
;(send frame show #t)
;
;
;(loop)
;
;(define (add-drawing p)
;  (let ([drawer (make-pict-drawer p)])
;    (new canvas% [parent frame]
;         [style '(border)]
;         [paint-callback (lambda (self dc)
;                           (drawer dc 0 0))])))
;(add-drawing zombie)




;(define target (make-bitmap 30 30)) ; A 30x30 bitmap
;(define dc (new bitmap-dc% [bitmap target]))
;(send dc draw-rectangle
;      0 10   ; Top-left at (0, 10), 10 pixels down from top-left
;      30 10) ; 30 pixels wide and 10 pixels high
;(send dc draw-line
;      0 0    ; Start at (0, 0), the top-left corner
;      30 30) ; and draw to (30, 30), the bottom-right corner
;(send dc draw-line
;      0 30   ; Start at (0, 30), the bottom-left corner
;      30 0)  ; and draw to (30, 0), the top-right corner
;(send dc set-brush "green" 'solid)
;(send dc set-pen "blue" 1 'solid)
;(send dc draw-rectangle 0 10 30 10)
;(send dc set-pen "red" 3 'solid)
;(send dc draw-line 0 0 30 30)
;(send dc draw-line 0 30 30 0)
;
;(define no-pen (new pen% [style 'transparent]))
;(define no-brush (new brush% [style 'transparent]))
;(define blue-brush (new brush% [color "blue"]))
;(define yellow-brush (new brush% [color "yellow"]))
;(define red-pen (new pen% [color "red"] [width 2]))
;
;(define (draw-face dc)
;  (send dc set-smoothing 'aligned)
;
;  (send dc set-pen no-pen)
;  (send dc set-brush blue-brush)
;  (send dc draw-ellipse 25 25 100 100)
;  
;  (send dc set-brush yellow-brush)
;  (send dc draw-rectangle 50 50 10 10)
;  (send dc draw-rectangle 90 50 10 10)
;  
;  (send dc set-brush no-brush)
;  (send dc set-pen red-pen)
;  (send dc draw-arc 37 37 75 75 (* 5/4 pi) (* 7/4 pi)))
;(draw-face dc)

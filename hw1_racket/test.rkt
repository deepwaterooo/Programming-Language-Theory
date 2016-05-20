#! /usr/bin/env racket

#lang racket/gui

(define frame (new frame% [label "Zombie Dancing"]
                   [width 500]
                   [height 500]))

(define msg (new message% [parent frame]
                 [label "No events so far yet..."]))


; Derive a new canvas (a drawing window) class to handle events
(define my-canvas%
  (class canvas%   ; The base class is canvas%
    ; Define overriding method to handle mouse events
    (define/override (on-event event)
      (send msg set-label "Canvas mouse"))
    ; Define overriding method to handle keyboard events
    (define/override (on-char event)
      (send msg set-label "Canvas keyboard"))
    ; Call the superclass init, passing on all init args
    (super-new)))

; Make a canvas that handles events in the frame
(new my-canvas% [parent frame]
     [paint-callback
      (lambda (canvas dc)
        (send dc set-scale 3 3)
        (send dc set-text-foreground "blue")
;        (send dc set-canvas-background "yellow")
        (send dc draw-text "Don't Panic!" 0 0))])


(define panel (new horizontal-panel% [parent frame]
                   [alignment '(center center)]))
(new button% [parent panel]
     [label "Start"]
     [callback (lambda (button event)
                 (send msg set-label "Button click"))])
(new button% [parent panel]
     [label "Pause"]
     [callback (lambda (button event)
                 (send msg set-label "Pause Button click")
                 (sleep 3))])
(send frame show #t)



(require racket/draw)
(define target (make-bitmap 30 30)) ; A 30x30 bitmap

(define dc (new bitmap-dc% [bitmap target]))

(send dc draw-rectangle
      0 10   ; Top-left at (0, 10), 10 pixels down from top-left
      30 10) ; 30 pixels wide and 10 pixels high
(send dc draw-line
      0 0    ; Start at (0, 0), the top-left corner
      30 30) ; and draw to (30, 30), the bottom-right corner
(send dc draw-line
      0 30   ; Start at (0, 30), the bottom-left corner
      30 0)  ; and draw to (30, 0), the top-right corner

(send dc set-brush "green" 'solid)
(send dc set-pen "blue" 1 'solid)
(send dc draw-rectangle 0 10 30 10)
(send dc set-pen "red" 3 'solid)
(send dc draw-line 0 0 30 30)
(send dc draw-line 0 30 30 0)

(require racket/math)

(define no-pen (new pen% [style 'transparent]))
(define no-brush (new brush% [style 'transparent]))
(define blue-brush (new brush% [color "blue"]))
(define yellow-brush (new brush% [color "yellow"]))
(define red-pen (new pen% [color "red"] [width 2]))

(define (draw-face dc)
  (send dc set-smoothing 'aligned)

  (send dc set-pen no-pen)
  (send dc set-brush blue-brush)
  (send dc draw-ellipse 25 25 100 100)
  
  (send dc set-brush yellow-brush)
  (send dc draw-rectangle 50 50 10 10)
  (send dc draw-rectangle 90 50 10 10)
  
  (send dc set-brush no-brush)
  (send dc set-pen red-pen)
  (send dc draw-arc 37 37 75 75 (* 5/4 pi) (* 7/4 pi)))
(draw-face dc)


;;; adapt to a zombie ~ me
;(define r (colorize (rectangle 20 20) "blue"))
;(define c (colorize (circle 20) "red"))
;(define mouse (colorize (rectangle 30 10) "black"))
;(vc-append 20 (hc-append 8 c r) mouse)

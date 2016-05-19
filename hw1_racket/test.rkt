#! /usr/bin/env racket

#lang slideshow

(let ([a 3]
      [b (list-ref '(1 2 3 4) 3)])
  (sqr (+ a b)))
;(or a b)
(let* ([x 10]
       [y (* x x)])
  (list x y))

(let-values ([(x y) (quotient/remainder 10 3)])
  (list y x))

(let*-values ([(pi r rr) (values 3.1415926 10
                                 (lambda (x) (* x x)))]
              [(perimeter area) (values (* 2 pi r) (* pi (rr r)))])
  (list area perimeter))

(letrec ([is-even? (lambda (n)
                     (or (zero? n)
                         (is-odd? (sub1 n))))]
         [is-odd? (lambda (n)
                    (and (not (zero? n))
                         (is-even? (sub1 n))))])
  (is-odd? 11))

(define ht (hash "key1" "value1" 'key2 1234 3 (list 1 2 3 4) (list 'key4) 'value4))
(hash-ref ht "key1")
(hash-ref ht 'key2)
(hash-ref ht 3)
(hash-ref ht (list 'key4))
;(hash-ref ht 'key5)


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



(define r (colorize (rectangle 20 20) "blue"))

(define c (colorize (circle 20) "red"))

(define mouse (colorize (rectangle 30 10) "black"))

(vc-append 20 (hc-append 8 c r) mouse)



#! /usr/bin/env slideshow

#lang slideshow

(define (square n)
  (filled-rectangle n n))

(define (checker p1 p2)
  (let ([p12 (hc-append p1 p2)]
        [p21 (hc-append p2 p1)])
    (vc-append p12 p21)))

(define (checkerboard p)
  (let* ([rp (colorize p "red")]
         [bp (colorize p "black")]
         [c (checker rp bp)]
         [c4 (four c)])
    (four c4)))

(define (four p)
  (define two-p (hc-append p p))
  (vc-append two-p two-p))

(checkerboard (square 4))

(define sumof
  (lambda (n)
    (/ (* n (+ n 1)) 2)))

(define sumRange
  (lambda (a b)
    (- (sumof b) (sumof a))))

(sumRange 3 4)

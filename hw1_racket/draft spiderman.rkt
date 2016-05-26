#! /usr/bin/env racket

#lang slideshow
(define y (colorize(filled-rectangle 10 10) "yellow"))
(define b (colorize(filled-rectangle 10 10) "blue"))
(define B (colorize(filled-rectangle 10 10) "black"))
(define r (colorize(filled-rectangle 10 10) "red"))
(define w (colorize(filled-rectangle 10 10) "white"))
(define rope (colorize(rectangle 10 10) "gray"))
(define spiderman
  (let (
      [r1  (hc-append y y y y y y y y y y y y y y y y y rope y y y y y y y y y y y y y y y y y y)]
      [r2  (hc-append y y y y y y y y y y y y y y y y y rope y y y y y y y y y y y y y y y y y y)]
      [r3  (hc-append y y y y y y y y y y y y y y y y y rope y y y y y y y y y y y y y y y y y y)]
      [r4  (hc-append y y y y y y y y y y y y y y y y B rope B y y y y y y y y y y y y y y y y y)]
      [r5  (hc-append y y y y y y y y y y y y y y y B r rope r B y y y y y y y y y y y y y y y y)]
      [r6  (hc-append y y y y y y y y y y y y y y B r r rope r r B y y y y y y y y y y y y y y y)]
      [r7  (hc-append y y y y y y y y y y y y y B r r r rope r r r B y y y y y y y y y y y y y y)]
      [r8  (hc-append y y y y y y y y y y y B B r r r r rope r r r r B B y y y y y y y y y y y y)]
      [r9  (hc-append y y y y y y y y y y B b r r r B B rope B B r r r b B y y y y y y y y y y y)]
      [r10 (hc-append y y y y y y y y y B b b b r B b rope rope b b B r b b b B y y y y y y y y y y)]
      [r11 (hc-append y y y y y y y y B b b b b B B B rope rope rope B B B b b b b B y y y y y y y y y)]
      [r12 (hc-append y y y y y y y y B b b b B r r r B rope B r r r B b b b B y y y y y y y y y)]
      [r13  (hc-append y y y y y y y y y B B B r r r r B r B r r r r B B B y y y y y y y y y y)]
      [r14  (hc-append y y y y y y y y y y y B r r b B r r r B b r r B y y y y y y y y y y y y)]
      [r15  (hc-append y y y y y y y y y y B r r b B r r r r b B b r r B y y y y y y y y y y y)]
      [r16  (hc-append y y y y y y y y y y B r r B b r r r r r b B r r B y y y y y y y y y y y)]
      [r17  (hc-append y y y y y y y y y y y B r r r r r r r r r r r B y y y y y y y y y y y y)]
      [r18  (hc-append y y y y y y y y y y y y B B r r B B B r r B B y y y y y y y y y y y y y)]
      [r19  (hc-append y y y y y y y y y y y y y y B B r r r B B y y y y y y y y y y y y y y y)]
      [r20  (hc-append y y y y y y y y y y y y y B r r r r r r r B y y y y y y y y y y y y y y)]
      [r21  (hc-append y y y y y y y y y y y y B r r r r r r r r r B y y y y y y y y y y y y y)]
      [r22  (hc-append y y y y y y y y y y y B r r r r r r r r r r r B y y y y y y y y y y y y)]
      [r23  (hc-append y y y y y y y y y y y B r r B B r r r B B r r B y y y y y y y y y y y y)]
      [r24  (hc-append y y y y y y y y y y y B r B w w B r B w w B r B y y y y y y y y y y y y)]
      [r27  (hc-append y y y y y y y y y y y B r B w B r r r B w B r B y y y y y y y y y y y y)]
      [r28  (hc-append y y y y y y y y y y y B r B B r r r r r B B r B y y y y y y y y y y y y)]
      [r29  (hc-append y y y y y y y y y y y B r B r r r r r r r B r B y y y y y y y y y y y y)]
      [r30  (hc-append y y y y y y y y y y y B r r r r r r r r r r r B y y y y y y y y y y y y)]
      [r31  (hc-append y y y y y y y y y y y B r r r r r r r r r r r B y y y y y y y y y y y y)]
      [r32  (hc-append y y y y y y y y y y y B r r r r r r r r r r r B y y y y y y y y y y y y)]
      [r33  (hc-append y y y y y y y y y y y y B r r r r r r r r r B y y y y y y y y y y y y y)]
      [r34  (hc-append y y y y y y y y y y y y y B B r r r r r B B y y y y y y y y y y y y y y)]
      [r35  (hc-append y y y y y y y y y y y y y y y B B B B B y y y y y y y y y y y y y y y y)]
      [r36  (hc-append y y y y y y y y y y y y y y y y y y y y y y y y y y y y y y y y y y y y)])
  (vc-append r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15 r16 r17 r18 r19 r20 r21 r22 r23 r24 r27 r28 r29 r30 r31 r32 r33 r34 r35 r36)))
spiderman


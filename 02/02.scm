#!/usr/bin/env -S guile -s
!#

;; hint: ./lisp-input.sed input.txt > input.scm

(define hpos 0)
(define vpos 0)

(define (forward n)
  (set! hpos (+ hpos n)))

(define (up n)
  (set! vpos (- vpos n)))

(define (down n)
  (set! vpos (+ vpos n)))

(load "input.scm")
(format #t "~%Part One Result: ~d~%" (* hpos vpos))

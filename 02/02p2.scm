#!/usr/bin/env -S guile -s
!#

;; hint: ./lisp-input.sed input.txt > input.scm

(define hpos 0)
(define vpos 0)
(define aim 0)

(define (forward n)
  (set! hpos (+ hpos n))
  (set! vpos (+ vpos (* aim n))))

(define (up n)
  (set! aim (- aim n)))

(define (down n)
  (set! aim (+ aim n)))

(load "input.scm")
(format #t "~%Part Two Result: ~d~%" (* hpos vpos))

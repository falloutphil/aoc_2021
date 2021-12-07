#!/usr/bin/env sh
exec guile -e '(@ (day01) main)' -s "$0" "$@"
!#

(define-module (day02)
  #:export (main))

(define hpos 0)
(define vpos 0)

(define (forward n)
  (set! hpos (+ hpos n)))

(define (up n)
  (set! hpos (- vpos n)))

(define (down n)
  (set! hpos (+ vpos n)))

(define (main args)
  )

#!/usr/bin/env -S guile -s
!#

(use-modules (system base compile)) ;; compile-file

;; requires wisp >= 1.0.7

(define hpos 0)
(define vpos 0)

(define (forward n)
  (set! hpos (+ hpos n)))

(define (up n)
  (set! vpos (- vpos n)))

(define (down n)
  (set! vpos (+ vpos n)))

(compile-file "input.txt" #:from 'wisp)
(load "input.txt")
(format #t "~%Result: ~a~%" (* hpos vpos))

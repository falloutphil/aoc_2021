#!/usr/bin/env -S guile -s
!#

;; requires wisp >= 1.0.7

(define hpos 0)
(define vpos 0)

(define (forward n)
  (set! hpos (+ hpos n)))

(define (up n)
  (set! vpos (- vpos n)))

(define (down n)
  (set! vpos (+ vpos n)))

(load "input.txt" (λ (p) ((@@ (language wisp spec) read-one-wisp-sexp) p #f)))
(format #t "~%Result: ~a~%" (* hpos vpos))

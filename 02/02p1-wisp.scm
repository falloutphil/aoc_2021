#!/usr/bin/env -S guile -s
!#

(use-modules ((srfi srfi-26)
	      #:select (cut)))

;; requires wisp >= 1.0.7

(define hpos 0)
(define vpos 0)

(define (forward n)
  (set! hpos (+ hpos n)))

(define (up n)
  (set! vpos (- vpos n)))

(define (down n)
  (set! vpos (+ vpos n)))

;; weirdly the wisp reader takes a 2nd env arg that is never used?
(load "input.txt"
      (cut (@@ (language wisp spec) read-one-wisp-sexp) <> #f))
(format #t "~%Result: ~a~%" (* hpos vpos))

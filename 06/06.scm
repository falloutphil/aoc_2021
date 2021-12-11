#!/usr/bin/env sh
exec guile -e '(@ (day06) main)' -s "$0" "$@"
!#

(define-module (day06)
  #:export (main)
  #:use-module (ice-9 rdelim) ;; read-line
  #:use-module (srfi srfi-1) ;; iota, last
  #:use-module (srfi srfi-26)) ;; cut

(define (get-input filename)
  "Take first line of filename as input.
   Convert to list of numbers."
  (map string->number
       (string-split (call-with-input-file filename read-line)
		     #\,)))

(define (fish-evolve fish)
  "A 0 becomes a 6, and adds a new 8."
  "Every other number is decreased by 1."
  (if (eqv? fish 0)
      '(6 8)
      (list (- fish 1))))

(define (school-evolve fishes day)
  "Tick forwards one day."
  ;(format #t "~%~%fish: ~a length: ~a~%" fishes (length fishes))
  (if (> day 80)
      (length fishes)
      (school-evolve (concatenate (map fish-evolve fishes)) (+ day 1))))

(define (main args)
  (let ((fishes (get-input "input.txt")))
    (format #t "~%~%part 1: ~a~%" (school-evolve fishes 1))))

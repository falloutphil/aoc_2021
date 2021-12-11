#!/usr/bin/env sh
exec guile -e '(@ (day06) main)' -s "$0" "$@"
!#

(define-module (day06)
  #:export (main)
  #:use-module (ice-9 rdelim) ;; read-line
  #:use-module (srfi srfi-1) ;; iota, last
  #:use-module (srfi srfi-11) ;; let-values
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
  (format #t "~%~%day: ~a length: ~a~%" day (length fishes))
  (if (>= day 80)
      (length fishes)
      (school-evolve (concatenate (map fish-evolve fishes)) (+ day 1))))

(define (fish-bloodline fishes day)
  ;;(format #t "~%~%day: ~a length: ~a~%" day (length fishes))
  (cond
   ((eqv? day 256)
    (length fishes))
   ((eqv? (euclidean-remainder (length fishes) 17) 0)
    (let-values (((f1 f2) (split-at fishes (euclidean-quotient (length fishes) 2))))
	  (+ (fish-bloodline (concatenate (map fish-evolve f1)) (+ day 1))
	     (fish-bloodline (concatenate (map fish-evolve f2)) (+ day 1)))))
   (else 
      (fish-bloodline (concatenate (map fish-evolve fishes)) (+ day 1)))))

(define (bloodline-mapper fish)
  (format #t "~%~%FISH: ~a~%" fish)
  (fish-bloodline fish 0))

(define (school-evolve-p2 fishes)
  "Tick forwards one day."
  (reduce + 0 (map bloodline-mapper (map list fishes))))
  
      

(define (main args)
  (let ((fishes (get-input "test_input.txt")))
    (format #t "~%~%part 1: ~a~%" (school-evolve-p2 fishes))))

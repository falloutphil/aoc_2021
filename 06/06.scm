#!/usr/bin/env sh
# -*- mode:scheme; geiser-scheme-implemenatation: guile -*-
exec guile -e '(@ (day06) main)' -s "$0" "$@"
!#

(define-module (day06)
  #:export (main)
  #:use-module (ice-9 rdelim) ;; read-line
  #:use-module (srfi srfi-1) ;; concatenate
  #:use-module (srfi srfi-11) ;; let-values
  #:use-module (srfi srfi-43)) ;; vector-fold

(define (get-input filename)
  "Take first line of filename as input.
   Convert to list of numbers."
  (map string->number
       (string-split (call-with-input-file filename read-line)
		     #\,)))

;; Part 1 - naive solution

(define (fish-evolve fish)
  "A 0 becomes a 6, and adds a new 8."
  "Every other number is decreased by 1."
  (if (eqv? fish 0)
      '(6 8)
      (list (- fish 1))))

(define (school-evolve fishes day)
  "Tick forwards one day."
  ;;(format #t "~%~%day: ~a length: ~a~%" day (length fishes))
  (if (>= day 80)
      (length fishes)
      (school-evolve (concatenate (map fish-evolve fishes)) (+ day 1))))

;; Initial attempt at Part 2 using a map reduce approach split and sum sublists of fish
;; It takes hours to run but is faster than the Part 1 solution!
;; It tries to solve each element in the list of fishes in isolation and stores only
;; length of the resulting bloodline (all descendants of that one fish).
;; This means resource use doesn't increase with input size.
;; It takes about an hour per fish - so about 10 days to do the puzzle! 
;; eg with test input (4.92 hours):
;; result: 26984457539
;; ./06.scm  17724.66s user 136.21s system 102% cpu 4:51:16.29 total
(define (fish-bloodline fishes day)
  ;;(format #t "~%~%day: ~a length: ~a~%" day (length fishes))
  (cond
   ((eqv? day 256)
    (length fishes))
   ((eqv? (euclidean-remainder (length fishes) 17) 0) ;; split when any list grows to 17 (seems to perform best)
    (let-values (((f1 f2) (split-at fishes (euclidean-quotient (length fishes) 2))))
	  (+ (fish-bloodline (concatenate (map fish-evolve f1)) (+ day 1)) ;; continue evolving with the list split in 2
	     (fish-bloodline (concatenate (map fish-evolve f2)) (+ day 1)))))
   (else 
      (fish-bloodline (concatenate (map fish-evolve fishes)) (+ day 1))))) ;; continue without splitting

(define (bloodline-mapper fish)
  ;;(format #t "~%~%FISH: ~a~%" fish)
  (fish-bloodline fish 0))

;; Entry point for Map Reduce solution
(define (school-evolve-mr fishes)
  "Map Reduce Attempt. Tick forwards one day."
  (reduce + 0 (map bloodline-mapper (map list fishes)))) ;; put each genesis-fish in it's own list and map it's bloodline


;; Part 2 much faster solution using vectors

(define (initialize-fish-vector fish-list)
  "Count the fish at each day in their cycle."
  (let ((fv (make-vector 9 0)))
    (for-each (lambda (day)
		(vector-set! fv day (+ (vector-ref fv day) 1)))
	      fish-list)
    fv))
    
(define (evolve-fish-vector fv day)
  "Tick one day.  Shift the fish left and record new births."
  ;;(format #t "~%~%fish: ~a day: ~a count: ~a~%" fv day (vector-fold (lambda (_ c n) (+ c n)) 0 fv))
  (if (eqv? day 256)
      (vector-fold (lambda (_ c n) (+ c n)) 0 fv) ;; done, sum all the fish counts for each day
      (let ((recycled-fish (vector-ref fv 0))) ;; represents mothers and babies
	(vector-move-left! fv 1 9 fv 0) ;; shift 1->0, 2->1, ...., 8->7.
	(vector-set! fv 6 (+ (vector-ref fv 6) recycled-fish)) ;; mother fish are added 
	(vector-set! fv 8 recycled-fish) ;; baby fish
	(evolve-fish-vector fv (+ day 1))))) 


(define (main args)
  (let ((fishes (get-input "input.txt")))
    (format #t "~%~%part 1: ~a~%" (school-evolve fishes 0))
    (format #t "~%~%part 2: ~a~%" (evolve-fish-vector
				   (initialize-fish-vector fishes) 0))))

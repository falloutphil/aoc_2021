#!/usr/bin/env sh
# -*- mode:scheme; geiser-scheme-implemenatation: guile -*-
exec guile -e '(@ (day11) main)' -s "$0" "$@"
!#

#!
GEISER NOTES

In src - eval buffer C-c C-b /// eval buffer and go C-c M-b
In src - documentation for symbol at point C-c C-d C-d
In repl - Set current module C-c C-m 
Quit repl - C-c C-q
In repl - Clear REPL C-c M-o 
Switch between src and repl (and back) C-c C-z

Load module switch and enter in repl:
M-x run-geiser C-c C-b C-c C-z C-c C-m

Or there is a shortcut - works even if repl isn't open:
C-c C-a

But I don't seem to be able to run it.

(main '())

,module or ,m - current module
,import - other imports apart from current module

https://www.gnu.org/software/guile/manual/guile.html#REPL-Commands


,trace or ,tr - trace expression
!#


#!
OCTOPUSES

Create list of list from file convert to 2d-array

10x10 grid

Then

+1 whole grid

filter points that >= 9

make subgrids around them and increment members

filter points that >=9 and continue with original grid <- doubly recursive.

THEN - Reset all flashed octopuses to 0.

Possible we'll need an array of classes, but start with int
!#


(define-module (day11)
  #:export (main)
  #:use-module (oop goops) 
  #:use-module (ice-9 rdelim) ;; read-line
  #:use-module (srfi srfi-1) ;; concatenate
  #:use-module (srfi srfi-26) ;; cut
  #:use-module (srfi srfi-42)) ;; list-ec/eager comprehensions

(define (parse-input filename)
  "Read file of numbers into 2D array."
  (call-with-input-file filename
    (lambda (p)
      (list->array 2 (list-ec (:port line p read-line)
			      (list-ec (:string ch line)
				       (- (char->integer ch) 48)))))))

(define (make-2d-coords arr)
  "Create a set of coordinate pairs for the given array."
  (let* ((dims (array-shape arr))
	 ;; get bounds in form iota likes
         (col-bounds (list (- (cadar dims) -1 (caar dims)) (caar dims)))
         (row-bounds (list (- (cadadr dims) -1 (caadr dims)) (caadr dims))))
    ;;(format #t "~%dims: ~a" dims)
    (let ((result (concatenate
                   (map (λ (col)
                          (map (λ (row) (list col row))
                               (apply iota row-bounds)))
                        (apply iota col-bounds)))))
      ;;(format #t "~%2d result: ~a" result)
      result)))


(define (neighbour-coords i j)
  `((,(1- i) ,(1- j)) (,(1- i) ,j) (,(1- i) ,(1+ j))
    (,i ,(1- j)) (,i ,(1+ j))
    (,(1+ i) ,(1- j)) (,(1+ i) ,j) (,(1+ i) ,(1+ j))))

(define (energise! arr)
  (array-map! arr 1+ arr))

(define (make-do-neighbours)
  (let ((counter 0))
    (λ (arr i j)
      (if (eq? arr #f)
	  counter
	  ;;(format #t "~%flasher: (~a ~a)" i j)
	  (if (>= (array-ref arr i j) 9) ;; is he a flasher?
	      (let ((neighbours (filter
				 (λ (n) (and (apply array-in-bounds? (cons arr n)) ;; legal coord?
					     (> (apply array-ref (cons arr n)) -1))) ;; ignore if -1 (already flashed)
				 (neighbour-coords i j))))
		(set! counter (1+ counter)) ;; increase the flash count
		(array-set! arr -1 i j) ;; reset the flasher, -1 will set it to 0 when it is engerised
		;;(format #t "~%neighbours: ~a" neighbours)
		(for-each (λ (coord) ;; increment the neighbours
			    ;;(format #t "~%coord: ~a" coord)
			    (apply array-set! `(,arr
						,(1+ (apply array-ref (cons arr coord)))
						,@coord)))
			  neighbours)
		(filter (λ (n) (>= (apply array-ref (cons arr n)) 9)) neighbours)) ;; return any new flashers
	      '()))))) ;; or nothing if he doesn't flash


(define (make-neighbour-flash! arr)
  (let ((do-neighbours (make-do-neighbours)))
    (λ (i j)
      (let recurse ((coords (list (list i j)))) ;; LoL of coords
	;;(format #t "~%arr: ~a" arr)
	;;(format #t "~%coord list: ~a" coords)
	(if (null? coords)
	    (do-neighbours #f #f #f) ;; return count
	    (recurse (append (let ((i (caar coords))
				   (j (cadar coords)))
			       (do-neighbours arr i j))
			     (cdr coords))))))))

#!
0 1 2 3 4
1
2   a b c
3   d x e
4   f g h
!#

(define (main args)
  (let* ((octopus-arr (parse-input "input.txt"))
	 (nf! (make-neighbour-flash! octopus-arr))
	 (coords (make-2d-coords octopus-arr))
	 (flash-count #f))
    (let loop ((n 100))
      (unless (zero? n)
	(begin
	  ;;(format #t "~%n: ~a" n)
	  ;; check for flashes and neighbour flashes
	  ;; the last coord (9 9) has the cumulative value of all
	  ;; grids so far, which I should improve!
	  (set! flash-count (last (map (cut apply nf! <>) coords)))
	  ;; +1 to whole grid
          (energise! octopus-arr)
	  ;;(format #t "~%current grid: ~a~%" octopus-arr)
	  (loop (1- n)))))
    (format #t "~%Part 1: ~a~%" flash-count)))

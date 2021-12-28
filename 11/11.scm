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
  #:use-module (srfi srfi-1) ;; concatenate, filter-map, compose, append-reverse
  #:use-module (srfi srfi-26) ;; cut
  #:use-module (srfi srfi-42)) ;; list-ec/eager comprehensions

(define (parse-input filename)
  "Read file of numbers into 2D array."
  (call-with-input-file filename
    (lambda (p)
      (list->array 2 (list-ec (:port line p read-line)
			      (list-ec (:string ch line)
				       (- (char->integer ch) 48)))))))

(define (neighbour-coords i j)
  `((,(1- i) ,(1- j)) (,(1- i) ,j) (,(1- i) ,(1+ j))
    (,i ,(1- j)) (,i ,(1+ j))
    (,(1+ i) ,(1- j)) (,(1+ i) ,j) (,(1+ i) ,(1+ j))))

(define (energise! arr)
  (array-map! arr 1+ arr))

(define (make-neighbour-flash! arr)
  (λ (i j)
    (if (>= (array-ref arr i j) 9)
	(let ((neighbours (filter
			   (λ (n) (apply array-in-bounds? (cons arr n)))
			   (neighbour-coords i j))))
	  (format #t "~%neighbours: ~a" neighbours)
	  (for-each (λ (coord)
		      (format #t "~%coord: ~a" coord)
		      (apply array-set! `(,arr
					  ,(1+ (apply array-ref (cons arr coord)))
					  ,@coord)))
		    neighbours))
	(format #t "~%value: ~a" (array-ref arr i j)))))

#!

0 1 2 3 4
1
2   a b c
3   d x e
4   f g h

!#

(define (main args)
  (let* ((octopus-arr (parse-input "test_input.txt"))
	 (nf! (make-neighbour-flash! octopus-arr)))
    (energise! octopus-arr)
    (format #t "~%~a~%" octopus-arr)
    (nf! 0 2)
    (format #t "~%~a~%" octopus-arr)))

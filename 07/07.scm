#!/usr/bin/env sh
exec guile -e '(@ (day07) main)' -s "$0" "$@"
!#

#!
Part 1 is finding the min(s), of the sum of the absolute distances on a set of points x.
For candidate s, what is fuel cost of moving all x to it, what is the smallest possible s?
The answer is the median of the dataset - for a good recap:
https://www.johnmyleswhite.com/notebook/2013/03/22/modes-medians-and-means-an-unifying-perspective/

We can either calculate each sum(abs distance) over the range of 0 to x_max.
Or as we have an even number of candidates we should be able to sort the list and pick the value halfway between the 2 central values.
!#

(define-module (day07)
  #:export (main)
  #:use-module (ice-9 rdelim) ;; read-line
  #:use-module (srfi srfi-1) ;; concatenate
  #:use-module (srfi srfi-11) ;; let-values
  #:use-module (srfi srfi-43)) ;; vector-fold

(define (get-input filename)
  "Take first line of filename as input.
   Convert to list of numbers."
  (list->vector (map string->number
       (string-split (call-with-input-file filename read-line)
		     #\,))))

(define (main args)
  ;; Let's use the mid-value of a sorted list of even elements method
  (let* ((sorted-locs (sort (get-input "input.txt") <))
	 (high-mid (/ (vector-length sorted-locs) 2)) ;; ignore 0 indexing to get high-mid
	 (median-pos (/ (+ (vector-ref sorted-locs (- high-mid 1)) ;; then decrement to get low-mid
			   (vector-ref sorted-locs high-mid))
			2)))
    (format #t "~%~%part 1: ~a~%"
		 (vector-fold
		  (lambda (_ count pos)
		    (+ count (abs (- pos median-pos)))) ;; fuel cost is abs distance
		  0 sorted-locs))))

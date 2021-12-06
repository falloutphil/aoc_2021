#!/usr/bin/env sh
exec guile -e '(@ (day01) main)' -s "$0" "$@"
!#

(define-module (day01)
  #:export (main)
  #:use-module (ice-9 rdelim) ;; read-line
  #:use-module (srfi srfi-1) ;; count, take
  #:use-module (srfi srfi-42)) ;; list-ec

(define (file->list filename init transform)
  "Read filename, and convert each line to a number.
   Then apply init to the whole list.
   Then call transform on each element."
  (call-with-input-file filename
    (lambda (p)
      (map transform
	   (init (map string->number ;; not wonderfully efficient
		      (list-ec (:port line p read-line) line)))))))

(define (make-count-increments)
  "Constructor for closure retaining state
   of the previous element for comparison."
  (let ((last-value #f))
    (lambda (depth-reading)
      (let* ((result (if last-value
			(- depth-reading last-value)
			0)))
	(set! last-value depth-reading)
	result))))

(define (sum-moving-window xs)
  "Sum moving window of length 3."
  (if (>= (length xs) 3)
      (cons (apply + (take xs 3)) (sum-moving-window (cdr xs)))
      '()))

;; change to macro with nicer syntax
;; do you want to use map transform or write a recursive function - current implementation takes at least 2 passes?
(define (main args)
  (let ((counter-p1 (make-count-increments))
	(counter-p2 (make-count-increments)))
    (format #t
	    "~%Part One Result: ~a~%"
	    (count positive? (file->list "input.txt" identity counter-p1)))
    (format #t
	    "~%Part Two Result: ~a~%"
	    (count positive? (file->list "input.txt" sum-moving-window counter-p2)))))

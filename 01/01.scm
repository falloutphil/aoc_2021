#!/usr/bin/env sh
exec guile -e '(@ (day01) main)' -s "$0" "$@"
!#

(define-module (day01)
  #:export (main)
  #:use-module (ice-9 rdelim) ;; read-line
  #:use-module (srfi srfi-1) ;; count
  #:use-module (srfi srfi-42)) ;; list-ec

(define (file->list filename transform)
  (call-with-input-file filename
    (lambda (p)
      (map transform (list-ec (:port line p read-line) line)))))

(define (make-count-increments)
  (let ((last-value #f))
    (lambda (str-depth-reading)
      (let* ((depth-reading (string->number str-depth-reading))
	     (result (if last-value
			(- depth-reading last-value)
			0)))
	(set! last-value depth-reading)
	result))))

;; change to macro with nicer syntax
;; do you want to use map transform or write a recursive function - current implementation takes at least 2 passes?
(define (main args)
  (let ((counter (make-count-increments)))
    (format #t
	    "~%Result: ~a~%"
	    (count positive? (file->list "input.txt" counter)))))

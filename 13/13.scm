#!/usr/bin/env sh
# -*- mode:scheme; geiser-scheme-implemenatation: guile -*-
exec guile -e '(@ (day13) main)' -s "$0" "$@"
!#

;; https://gitlab.com/guile-syntax-parse/guile-syntax-parse
(add-to-load-path  "/home/phil/installs/guile-syntax-parse")

(define-module (day13)
  #:export (main)
  #:use-module (syntax parse)
  #:use-module (ice-9 rdelim) ;; read-line
  #:use-module (ice-9 match)
  #:use-module (ice-9 hash-table)
  #:use-module (srfi srfi-1) ;; last
  #:use-module (srfi srfi-11) ;; let-values
  #:use-module (srfi srfi-26) ;; cut
  #:use-module (srfi srfi-42)) ;; list-ec/eager comprehensions


;; Macro to split the result of file->list
;; into a list of coords and a list of folds.
(define parse-input
  (syntax-parser
   [((x y) ...+ #f folds ...+)     ;; input is (row col)
    #'(((y x) ...) (folds ...))])  ;; arrays are (col row)
  )


;; https://stackoverflow.com/a/70545115/2904770
(define (handle-fold line var)
  "Can be used as a cond test which returns a list
   of folds as truth (meaning no expression is needed)."
  (let ((str (string-append "fold along " (symbol->string var) "=")))
    (and (string= line str 1 13 1 13)
         (list var ((compose string->number
			     last
			     (cut string-split <> #\=))
		    line)))))


(define (file->list filename)
  "Read input and split into list of 
   coordinates and folds."
  (let ((lst (call-with-input-file filename
	       (λ (p)
		 (list-ec (:port line p read-line)
			  (cond
			   ((string-any (cut eqv? <> #\,) line)
			    (map string->number (string-split line #\,)))
			   ((string-null? line) #f) ;; blank line
			   ((handle-fold line 'x))
			   ((handle-fold line 'y))
			   (else (error "bad input!"))))))))
    (parse-input lst)))


(define (dimensions coords)
  "Get dimensions of a set of coords.
   Assumes points are in (col row) format."
  (let-values ([(col row) (unzip2 coords)])
    `(,(apply max col) ,(apply max row))))


(define (make-paper-array max-col max-row)
  "Created 2D bit-array to represent our initial flat piece of paper."
  (make-typed-array 'b #f `(0 ,max-col) `(0 ,max-row)))

(define (add-points-to-paper paper coords)
  "Add points to the initial paper."
  (for-each (λ (c)
	      (apply array-set! (append `(,paper #t) c)))
	    coords))

(define (fold-paper-arrays paper fold)
  (match-let* ([(axis coord) fold]
	       [(y-bounds x-bounds) (array-shape paper)]
	       [(y-low y-high) y-bounds]
	       [(x-low x-high) x-bounds]
	       [(side1 side2) (case axis
				[(x) (list (make-shared-array paper list y-bounds (list x-low (1- coord)))
					   (make-shared-array paper list y-bounds (list (1+ coord) x-high)))]
				[(y) (list (make-shared-array paper
							      list
							      (list y-low (1- coord)) x-bounds)
					   (make-shared-array paper
							      (λ (y x) (list (- (* 2 coord) y) x))
							      (list y-low (1- coord)) x-bounds))]
				[else error "bad axis!"])])
    (apply array-map! (append (list side1 (λ (e1 e2) (or e1 e2)) side1 side2)))))

(define (main args)
  (match-let* ([(coords folds) (file->list "test_input.txt")]
	       [(max-col max-row) (dimensions coords)])
    (format #t "~%coords: ~a~%" coords)
    (format #t "~%folds: ~a~%" folds)
    (format #t "~%dims - col: ~a row: ~a~%" max-col max-row)
    (let ([paper (make-paper-array max-col max-row)])
      (add-points-to-paper paper coords)
      (format #t "~%paper: ~a~%" paper)
      (fold-paper-arrays paper (car folds))
      (format #t "~%result: ~a~%" paper))))


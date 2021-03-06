#!/usr/bin/env sh
# -*- mode:scheme; geiser-scheme-implemenatation: guile -*-
exec guile -e '(@ (day13) main)' -s "$0" "$@"
!#

;; https://gitlab.com/guile-syntax-parse/guile-syntax-parse
(add-to-load-path "/home/phil/installs/guile-syntax-parse")

(define-module (day13)
  #:export (main)
  #:use-module (syntax parse)  ;; syntax-parser
  #:use-module (ice-9 rdelim)  ;; read-line
  #:use-module (ice-9 match) 
  #:use-module (srfi srfi-1)   ;; last
  #:use-module (srfi srfi-11)  ;; let-values
  #:use-module (srfi srfi-26)  ;; cut
  #:use-module (srfi srfi-42)) ;; list-ec/eager comprehensions


;; Macro to split the result of file->list
;; into a list of coords and a list of folds.
(define parse-input
  (syntax-parser
   [((x y) ...+ #f folds ...+)     ;; input is (row col)
    #'(((y x) ...) (folds ...))])) ;; arrays are (col row)



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
    (list (apply max col) (apply max row))))


(define (make-paper-array max-col max-row)
  "Created 2D bit-array to represent our initial flat piece of paper."
  (make-typed-array 'b #f `(0 ,max-col) `(0 ,max-row)))

(define (add-points-to-paper paper coords)
  "Add points to the initial paper."
  (for-each (λ (c)
	      (apply array-set! (append `(,paper #t) c)))
	    coords))


(define (create-folded-arrays paper transform y-bound x-bound)
  "Create 2 shared arrays, either side of the fold,
   and map their coordinate systems via transform."
  (list (make-shared-array paper
			   list ;; near-side always maintains original coord system
			   y-bound x-bound)
	(make-shared-array paper
			   transform ;; far-side counts back from far edge
			   y-bound x-bound)))


(define (fold-paper-arrays fold-pair paper)
  "Applies a single fold along either axis
   and update the original array with the
   points folded onto it."
  (match-let* ([(axis coord) fold-pair]
	       [(y-bound x-bound) (array-dimensions paper)]
	       [map-edge (cut - (* 2 coord) <>)] ;; count back from twice the distance from the fold
	       [(side1 side2) (case axis
				[(x) (create-folded-arrays paper
							   (λ (y x) (list y (map-edge x)))
							   y-bound coord)]
				[(y) (create-folded-arrays paper
							   (λ (y x) (list (map-edge y) x))
							   coord x-bound)]
				[else error "bad axis!"])])
    (array-map! side1
		(λ (e1 e2) (or e1 e2))
		side1 side2)
    ;; return a ref to the original, updated, folded portion as a shared array.
    ;; note no new arrays are ever created or destroyed for efficiency.
    ;; we just use an ever-decreasing portion of the original array.
    side1))

(define (main args)
  ;; Part 1
  (match-let* ([(coords folds) (file->list "part1_input.txt")]
	       [(max-col max-row) (dimensions coords)])
    ;;(format #t "~%coords: ~a~%" coords)
    ;;(format #t "~%folds: ~a~%" folds)
    ;;(format #t "~%dims - col: ~a row: ~a~%" max-col max-row)
    (let ([paper (make-paper-array max-col max-row)])
      (add-points-to-paper paper coords)
      (format #t "~%Part 1: ~a~%" (count identity
					 (concatenate
					  (array->list
					   (fold fold-paper-arrays paper folds)))))))
  ;; Part 2
  (match-let* ([(coords folds) (file->list "input.txt")]
	       [(max-col max-row) (dimensions coords)])
    (let ([paper (make-paper-array max-col max-row)])
      (add-points-to-paper paper coords)
      ;; 9608 is unicode "full block" in decimal.
      ;; ~Nc renders char code N.
      ;; ~:[F~;T~] treats input as bool and renders F for false, otherwise T.
      ;; ~{X~} iterates over the list input, it's nested with a newline.
      (format #t "~%Part 2:~%~{~{~:[ ~;~9608c~]~}~%~}~%"
	      (array->list (fold fold-paper-arrays paper folds))))))

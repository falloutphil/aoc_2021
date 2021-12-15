#!/usr/bin/env sh
exec guile -e '(@ (day09) main)' -s "$0" "$@"
!#

(define-module (day09)
  #:export (main)
  #:use-module (ice-9 rdelim) ;; read-line
  #:use-module (srfi srfi-1) ;; concatenate
  #:use-module (srfi srfi-11) ;; let-values
  #:use-module (srfi srfi-26) ;; cut
  #:use-module (srfi srfi-42)) ;; list-ec

(define (parse-input filename)
  "Read file of numbers into 2D array."
  (call-with-input-file filename
    (lambda (p)
      (list->array 2 (map (lambda (str)
			    (map (compose string->number string) (string->list str)))
		      (list-ec (:port line p read-line) line))))))

;; j-1 < 0, i-1 < 0 ---- top left make array (i i+1) (j j+1)
;; j-1 < 0 ---- left edge of row  make array (i-1 i+1) (j j+1)
;; i-1 < 0 ---- top row  make array (i i+1) (j-1 j+1)
;; j-1->j+1 and i-1->i+1 exists, make array (i-1 i+1) (j-1 j+1)
;; j+1 > len, i+1 > len ---- bottom right make array (i-1 i) (j-1 j)
;; j+1 > len ---- right edge of row make array (i-1 i+1) (j-1 j)
;; i+1 > len ---- bottom row make array (i-1 i) (j-1 j+1)
;; use check bounds so we can use same routine for all situations
(define (list+inc i)
  (list i (1+ i)))

(define (list+dec i)
  (list (1- i) i))

(define (list+-inc i)
  (list (1- i) (1+ i)))

(define (make-sub-grid world)
  (lambda (i-bounds j-bounds)
    ;; list is just a pass-through so we don't change
    ;; the coordinate system.  The idea is to use
    ;; array-for-each or similar to traverse the array
    ;; without coordinates.
    ;; The trick will be filtering out all the diagonals!
    (format #t "~%i bounds: ~a j bounds: ~a" i-bounds j-bounds)
    (make-shared-array world list i-bounds j-bounds)))

(define (adjacent-grid world i j)
  "Return grid centered on (i j)."
  (let* ((dims (array-dimensions world))
	 (cols (1- (cadr dims)))
	 (rows (1- (car dims)))
	 (sub-grid (make-sub-grid world)))
    (format #t "~%cols: ~a rows: ~a" cols rows)
    (cond
     ;; top left
     ((and (< (1- i) 0) (< (1- j) 0)) (begin (display " TOP LEFT ") (sub-grid (list+inc i) (list+inc j))))
     ;; top right
     ((and (< (1- i) 0) (> (1+ j) cols)) (begin (display " TOP RIGHT ") (sub-grid (list+inc i) (list+dec j))))
     ;; bottom left
     ((and (> (1+ i) rows) (< (1- j) 0)) (begin (display " BOTTOM LEFT ") (sub-grid (list+dec i) (list+inc j))))
     ;; bottom right
     ((and (> (1+ i) rows) (> (1+ j) cols)) (begin (display " BOTTOM RIGHT ") (sub-grid (list+dec i) (list+dec j))))
     ;; top row
     ((< (1- i) 0) (begin (display " TOP ROW ") (sub-grid (list+inc i) (list+-inc j))))
     ;; bottow row
     ((> (1+ i) rows) (begin (display " BOTTOM ROW ") (sub-grid (list+dec i) (list+-inc j))))
     ;; left col
     ((< (1- j) 0) (begin (display " LEFT COL ") (sub-grid (list+-inc i) (list+inc j))))
     ;; right col
     ((> (1+ j) cols) (begin (display " RIGHT COL ") (sub-grid (list+-inc i) (list+dec j))))
     ;; elsewhere, not on an edge
     (else (begin (display " OTHER ") (sub-grid (list+-inc i) (list+-inc j)))))))

(define (main args)
  (let* ((world (parse-input "test_input.txt"))
	 (test-grid (adjacent-grid world 4 2)))
    (format #t "~%world: ~a~%" world)
    (format #t "~%test-grid: ~a~%" test-grid)))


#!
scheme@(guile-user)> (make-shared-array a (lambda (i j) (list i j)) '(0 2) '(0 2))
$6 = #2((2 1 9) (3 9 8) (9 8 5))
scheme@(guile-user)> (make-shared-array a (lambda (i j) (list i j)) '(0 2) '(1 3))
$7 = #2@0@1((1 9 9) (9 8 7) (8 5 6))
scheme@(guile-user)> (make-shared-array a (lambda (i j) (list i j)) '(0 2) '(2 4))
!#

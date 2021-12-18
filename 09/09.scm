#!/usr/bin/env sh
exec guile -e '(@ (day09) main)' -s "$0" "$@"
!#

#!
Details of masks required on sub arrays to ignore diagonals.

c  = centre (#f in the actual arrays)
#t = point to compare to low point
#f = ignore this point

TOP LEFT

c  #t

#t #f

TOP RIGHT

#t  c

#f  #t

BOTTOM LEFT

#t  #f

#c  #t

BOTTOM RIGHT

#f  #f

#t  #c

LEFT COL

#t #f

c  #t

#t #f

RIGHT COL

#f #t

#t c

#f #t

TOP ROW

#t c  #t
#f #t #f

BOTTOM ROW

#f #t #f
#t c  #t

OTHERWISE

#f #t #f
#t c  #t
#f #t #f

!#

(define-module (day09)
  #:export (main)
  #:use-module (ice-9 rdelim) ;; read-line
  #:use-module (srfi srfi-1) ;; concatenate, iota, delete-duplicates
  #:use-module (srfi srfi-9) ;; records
  #:use-module (srfi srfi-26) ;; cut
  #:use-module (srfi srfi-42)) ;; list-ec

(define (parse-input filename)
  "Read file of numbers into 2D array."
  (call-with-input-file filename
    (lambda (p)
      (list->array 2 (map (lambda (str)
                            (map (compose string->number string) (string->list str)))
                          (list-ec (:port line p read-line) line))))))

;; Holds masking grid and it's central location.
(define-record-type <centred-mask>
  (make-centred-mask grid i j)
  centred-mask?
  (grid centred-mask-grid)
  (i centred-mask-i)
  (j centred-mask-j))

(define (zero-array-origin arr)
  "A naff way of resetting the array origin to (0 0)."
  (list->array 2 (array->list arr)))

;; not convinced about origin-translation!
(define (make-sub-grid world origin-translation)
  "Returns a function that takes a slice of the large array."
  (lambda (i-bounds j-bounds)
    ;; list is just a pass-through so we don't change
    ;; the coordinate system.  The idea is to use
    ;; array-for-each or similar to traverse the array
    ;; without coordinates.
    ;; The trick will be filtering out all the diagonals!
    ;;(format #t "~%i bounds: ~a j bounds: ~a" i-bounds j-bounds)
    (origin-translation (make-shared-array world list i-bounds j-bounds))))


;; Arrays returned by make-sub-grid are regular/rectangular arrays,
;; use masks to highlight the elements inside each slice that we
;; want to compare to the low point. This varies at the edges of the
;; large array (see block comment a top).

(define top-left
  (make-centred-mask #2((#f #t) (#t #f)) 0 0))

(define top-right
  (make-centred-mask #2((#t #f) (#f #t)) 0 1))

(define bottom-left
  (make-centred-mask #2((#t #f) (#f #t)) 1 0))

(define bottom-right
  (make-centred-mask #2((#f #t) (#t #f)) 0 1))

(define top-row
  (make-centred-mask #2((#t #f #t) (#f #t #f)) 0 1))

(define bottom-row
  (make-centred-mask #2((#f #t #f) (#t #f #t)) 1 1))

(define left-col
  (make-centred-mask #2((#t #f) (#f #t) (#t #f)) 1 0))

(define right-col
  (make-centred-mask #2((#f #t) (#t #f) (#f #t)) 1 1))

(define other
  (make-centred-mask #2((#f #t #f) (#t #f #t) (#f #t #f)) 1 1))


(define (low-point? world coords)
  "Are the coords a low-point on the world array?"
  (let* ((result #t)
         (grid-pair (adjacent-grid world coords zero-array-origin)) ;; mask and subarray as pair
         (sub (cdr grid-pair))
         (c-mask (car grid-pair))
         ;; this is the reason we have to 0-anchor our arrays which is expensive :-(
         (c (array-ref sub (centred-mask-i c-mask) (centred-mask-j c-mask)))) ;; value @ centre
    ;;(format #t "~%world coords: (~a, ~a)" i j)
    ;;(format #t "~%grid: ~a" (centred-mask-grid c-mask))
    ;;(format #t "~%i: ~a" (centred-mask-i c-mask))
    ;;(format #t "~%j: ~a"  (centred-mask-j c-mask))
    ;;(format #t "~%centre: ~a" c)
    (array-for-each (lambda (sub-v mask-v)
                      ;;(format #t "~%sub-v: ~a mask-v: ~a" sub-v mask-v)
                      (when mask-v (set! result (and result (< c sub-v))))) ;; the key line!
                    sub (centred-mask-grid c-mask))
    result))

;; Helper functions for finding bounds
;; based on the centre point.

(define (list+inc i)
  (list i (1+ i)))

(define (list+dec i)
  (list (1- i) i))

(define (list+-inc i)
  (list (1- i) (1+ i)))


;; Create the slice from the world and associates
;; it with a mask of points to check.
(define (adjacent-grid world coords origin-translation)
  "Return grid centered on (i j)."
  (let* ((i (car coords))
         (j (cadr coords))
         (dims (array-dimensions world))
         (cols (1- (cadr dims)))
         (rows (1- (car dims)))
         (sub-grid (make-sub-grid world origin-translation)))
    ;;(format #t "~%cols: ~a rows: ~a" cols rows)
    (cond ;; order is important!
     ;; top left
     ((and (< (1- i) 0) (< (1- j) 0)) `(,top-left . ,(sub-grid (list+inc i) (list+inc j))))
     ;; top right
     ((and (< (1- i) 0) (> (1+ j) cols)) `(,top-right . ,(sub-grid (list+inc i) (list+dec j))))
     ;; bottom left
     ((and (> (1+ i) rows) (< (1- j) 0)) `(,bottom-left . ,(sub-grid (list+dec i) (list+inc j))))
     ;; bottom right
     ((and (> (1+ i) rows) (> (1+ j) cols)) `(,bottom-right . ,(sub-grid (list+dec i) (list+dec j))))
     ;; top row
     ((< (1- i) 0) `(,top-row . ,(sub-grid (list+inc i) (list+-inc j))))
     ;; bottow row
     ((> (1+ i) rows) `(,bottom-row . ,(sub-grid (list+dec i) (list+-inc j))))
     ;; left col
     ((< (1- j) 0) `(,left-col . ,(sub-grid (list+-inc i) (list+inc j))))
     ;; right col
     ((> (1+ j) cols) `(,right-col . ,(sub-grid (list+-inc i) (list+dec j))))
     ;; elsewhere, not on an edge
     (else `(,other . ,(sub-grid (list+-inc i) (list+-inc j)))))))

(define (make-2d-coords arr)
  "Create a set of coordinate pairs for the given array."
  (let* ((dims (array-shape arr))
         (col-bounds (list (- (cadar dims) -1 (caar dims)) (caar dims)))
         (row-bounds (list (- (cadadr dims) -1 (caadr dims)) (caadr dims))))
    (format #t "~%dims: ~a" dims)
    (let ((result (concatenate
                   (map (lambda (col)
                          (map (lambda (row) (list col row))
                               (apply iota row-bounds)))
                        (apply iota col-bounds)))))
      (format #t "~%2d result: ~a" result)
      result)))

(define (make-upstream? centre)
  (let ((traversed-coords '()))
    (lambda (element mask-element coord)
      (format #t "~%traversed-coords: ~a" traversed-coords)
      (let ((result (cond
		     ((eqv? element 9) #f)
		     ((member coord traversed-coords) #f) 
		     ((< element centre) #f) ;; if the candidate is less than the centre, it must be downstream
		     ((not mask-element) #f) ;; ignore diagonals
		     (else (begin (format #t "~%next point found: ~a" coord) coord)))))
	(set! traversed-coords (cons coord traversed-coords))
	result))))
 

(define (process world low-point)
  (format #t "~%low-point ~a" low-point)
  (let* ((adjacent (adjacent-grid world low-point identity)) ;; make zero-array calls all local like below
         (mask (car adjacent))
         (grid (cdr adjacent))
         (c (array-ref (zero-array-origin grid) (centred-mask-i mask) (centred-mask-j mask)))
         (upstream? (make-upstream? c))) ;; value @ centre)
    (format #t "~%grid: ~a" grid)
    (filter-map upstream?
                (concatenate (array->list grid))
                (concatenate (array->list (centred-mask-grid mask)))
                (make-2d-coords grid))))

(define (recurse-basins world low-points)
  (if (null? low-points)
      '()
      (let ((result (process world (car low-points))))
        (cons (cons result (recurse-basins world result))
              (recurse-basins world (cdr low-points))))))

(define (flatten1 lists)               
  (if (not (pair? lists))
      (list lists)
      (fold (lambda (right left)
              (append left (flatten1 right)))
            '()
            lists)))

(define (flatten lists)
  ;;(format #t "~%lists: ~a car: ~a" lists (number? (car lists)))
  (cond
   ((null? lists) (begin (display " MOO ") '()))
   ((and (pair? lists)
	 (pair? (car lists))
	 (number? (caar lists))) lists)
   ((pair? lists) (begin (display " ELSE ")
			 (append (flatten (car lists))
				 (flatten (cdr lists)))))
   (else (error "oops"))))

(define (main args)
  (let* ((world (parse-input "input.txt"))
         (world-coord-pairs (make-2d-coords world)))
    ;;(format #t "~%world: ~a~%" world)
    ;;(format #t "~%world array dimensions: ~a~%" world-coord-pairs)
    (let ((low-points (filter (lambda (coords)
                                (let ((result (low-point? world coords)))
                                  ;;(format #t "~%filter result: ~a~%" result)
                                  result))
                              world-coord-pairs)))
      (format #t "~%~%Part 1: ~a~%"
              (fold (lambda (p sum)
                      (+ sum 1 (array-ref world (car p) (cadr p))))
                    0
                    low-points))
      
      (format #t "~%~%Part 2: ~a~%" ((compose (cut apply * <>) (cut take <> 3) (cut sort <> >))
				     (map (compose length delete-duplicates flatten)
					  (map append (recurse-basins world low-points)
					       (map list low-points)))))
      ;; add low-points to the basin
      ;;(format #t "~%~%Part 2: ~a~%" (map (lambda (lpb) (count null? (concatenate lpb))) (recurse-basins world low-points)))
      )))


#!

Get the filtered list of low-points.

We're going to cons our way through it, relacing each low point with a set of coords.

For each low point get the adjacent grid coords.

Filter out 9s

length of list left
0 '()
else (map fn new list)

 | 0, 1, 2, 3, 4, 5, 6, 7, 8, 9
 | ----------------------------
0| 2, 1, 9, 9, 9, 4, 3, 2, 1, 0
1| 3, 9, 8, 7, 8, 9, 4, 9, 2, 1
2| 9, 8, 5, 6, 7, 8, 9, 8, 9, 2
3| 8, 7, 6, 7, 8, 9, 6, 7, 8, 9
4| 9, 8, 9, 9, 9, 6, 5, 6, 7, 8


 | 0, 1, 2, 3, 4, 5, 6, 7, 8, 9
 | ----------------------------
0| *, !, 9, 9, 9, *, *, *, *, !
1| *, 9, *, *, *, 9, *, 9, *, *
2| 9, *, *, *, *, *, 9, *, 9, 2
3| *, *, *, *, *, 9, *, *, *, 9
4| 9, *, 9, 9, 9, *, !, *, *, *


There are 2 issues

- low points themselves are not always recorded
- there is repition in the answer - which are correct duplicates

 | 00, 01, 02, 03, 04, 05, 06, 07, 08, 09, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20
 | ----------------------------------------------------------------------------------
0| 09, 09, 07, 06, 07, 08, 06, 07, 08, 09, 04, 03, 09, 03, 01, 03, 06, 07, 08, 09, 09
1| 09, 07, 05, 04, 03, 09, 05, 06, 07, 08, 09, 09, 08, 07, 05, 04, 05, 09, 09, 08, 09
2| 09, 08, 04, 03, 02, 03, 04, 05, 08, 09, 08, 09, 09, 08, 06, 06, 06, 09, 08, 07, 08
3| 08, 06, 05, 04, 07, 08, 05, 07, 08, 09, 07, 08, 09, 09, 07, 09, 08, 09, 07, 06, 04
4| 09, 08, 06, 05, 06, 09, 06, 08, 09, 05, 06, 07, 08, 09, 08, 09, 09, 09, 09, 07, 03

 | 00, 01, 02, 03, 04, 05, 06, 07, 08, 09, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20
 | ----------------------------------------------------------------------------------
0| 09, 09, 07, 06, 07, 08, 06, 07, 08, 09, **, !!, 09, **, !!, **, **, **, **, 09, 09
1| 09, 07, 05, 04, 03, 09, 05, 06, 07, 08, 09, 09, **, **, **, **, 05, 09, 09, 08, 09
2| 09, 08, 04, 03, 02, 03, 04, 05, 08, 09, 08, 09, 09, **, **, **, 06, 09, 08, 07, 08
3| 08, 06, 05, 04, 07, 08, 05, 07, 08, 09, 07, 08, 09, 09, 07, 09, 08, 09, 07, 06, 04
4| 09, 08, 06, 05, 06, 09, 06, 08, 09, 05, 06, 07, 08, 09, 08, 09, 09, 09, 09, 07, 03



9976786789439313678999989767678999865435679398654323678999987654313468954569865334568916987643236789
9754395678998754599898978954567999976556789498775413478998798743203457893479973212479425698432145799
9843234589899866698786767943456789987687899987653104567897659957312368931298984596599934679549234689
8654785789789979897645459892967895698798999999873215698958349876433456892987899989989895998998965899
9865696895678989999731346799898954569999888921954524899542129986556767999976998679767699767897896789

low-point (0 11)
grid: #2@0@10((4 3 9) (9 9 8))
dims: ((0 1) (10 12))
2d result: ((0 10) (0 11) (0 12) (1 10) (1 11) (1 12))
next point found: (0 10)

low-point (0 10)
grid: #2@0@9((9 4 3) (8 9 9))
dims: ((0 1) (9 11))
2d result: ((0 9) (0 10) (0 11) (1 9) (1 10) (1 11))

low-point (0 14)
grid: #2@0@13((3 1 3) (7 5 4))
dims: ((0 1) (13 15))
2d result: ((0 13) (0 14) (0 15) (1 13) (1 14) (1 15))
next point found: (0 13)
next point found: (0 15)
next point found: (1 14)

low-point (0 13)
grid: #2@0@12((9 3 1) (8 7 5))
dims: ((0 1) (12 14))
2d result: ((0 12) (0 13) (0 14) (1 12) (1 13) (1 14))
next point found: (1 13)

low-point (1 13)
grid: #2@0@12((9 3 1) (8 7 5) (9 8 6))
dims: ((0 2) (12 14))
2d result: ((0 12) (0 13) (0 14) (1 12) (1 13) (1 14) (2 12) (2 13) (2 14))
next point found: (1 12)
next point found: (2 13)

low-point (1 12)
grid: #2@0@11((3 9 3) (9 8 7) (9 9 8))
dims: ((0 2) (11 13))
2d result: ((0 11) (0 12) (0 13) (1 11) (1 12) (1 13) (2 11) (2 12) (2 13))

low-point (2 13)
grid: #2@1@12((8 7 5) (9 8 6) (9 9 7))
dims: ((1 3) (12 14))
2d result: ((1 12) (1 13) (1 14) (2 12) (2 13) (2 14) (3 12) (3 13) (3 14))

low-point (0 15)
grid: #2@0@14((1 3 6) (5 4 5))
dims: ((0 1) (14 16))
2d result: ((0 14) (0 15) (0 16) (1 14) (1 15) (1 16))
next point found: (0 16)
next point found: (1 15)

low-point (0 16)
grid: #2@0@15((3 6 7) (4 5 9))
dims: ((0 1) (15 17))
2d result: ((0 15) (0 16) (0 17) (1 15) (1 16) (1 17))
next point found: (0 17)

low-point (0 17)
grid: #2@0@16((6 7 8) (5 9 9))
dims: ((0 1) (16 18))
2d result: ((0 16) (0 17) (0 18) (1 16) (1 17) (1 18))
next point found: (0 18)

low-point (0 18)
grid: #2@0@17((7 8 9) (9 9 8))
dims: ((0 1) (17 19))
2d result: ((0 17) (0 18) (0 19) (1 17) (1 18) (1 19))

low-point (1 15)
grid: #2@0@14((1 3 6) (5 4 5) (6 6 6))
dims: ((0 2) (14 16))
2d result: ((0 14) (0 15) (0 16) (1 14) (1 15) (1 16) (2 14) (2 15) (2 16))
next point found: (1 14)
next point found: (1 16)
next point found: (2 15)

low-point (1 14)
grid: #2@0@13((3 1 3) (7 5 4) (8 6 6))
dims: ((0 2) (13 15))
2d result: ((0 13) (0 14) (0 15) (1 13) (1 14) (1 15) (2 13) (2 14) (2 15))
next point found: (1 13)
next point found: (2 14)

low-point (1 13)
grid: #2@0@12((9 3 1) (8 7 5) (9 8 6))
dims: ((0 2) (12 14))
2d result: ((0 12) (0 13) (0 14) (1 12) (1 13) (1 14) (2 12) (2 13) (2 14))
next point found: (1 12)
next point found: (2 13)

low-point (1 12)
grid: #2@0@11((3 9 3) (9 8 7) (9 9 8))
dims: ((0 2) (11 13))
2d result: ((0 11) (0 12) (0 13) (1 11) (1 12) (1 13) (2 11) (2 12) (2 13))

low-point (2 13)
grid: #2@1@12((8 7 5) (9 8 6) (9 9 7))
dims: ((1 3) (12 14))
2d result: ((1 12) (1 13) (1 14) (2 12) (2 13) (2 14) (3 12) (3 13) (3 14))

low-point (2 14)
grid: #2@1@13((7 5 4) (8 6 6) (9 7 9))
dims: ((1 3) (13 15))
2d result: ((1 13) (1 14) (1 15) (2 13) (2 14) (2 15) (3 13) (3 14) (3 15))
next point found: (2 13)
next point found: (2 15)
next point found: (3 14)

low-point (2 13)
grid: #2@1@12((8 7 5) (9 8 6) (9 9 7))
dims: ((1 3) (12 14))
2d result: ((1 12) (1 13) (1 14) (2 12) (2 13) (2 14) (3 12) (3 13) (3 14))

low-point (2 15)
grid: #2@1@14((5 4 5) (6 6 6) (7 9 8))
dims: ((1 3) (14 16))
2d result: ((1 14) (1 15) (1 16) (2 14) (2 15) (2 16) (3 14) (3 15) (3 16))
next point found: (2 14)
next point found: (2 16)

low-point (2 14)
grid: #2@1@13((7 5 4) (8 6 6) (9 7 9))
dims: ((1 3) (13 15))
2d result: ((1 13) (1 14) (1 15) (2 13) (2 14) (2 15) (3 13) (3 14) (3 15))
next point found: (2 13)
next point found: (2 15)
next point found: (3 14)

low-point (2 13)
grid: #2@1@12((8 7 5) (9 8 6) (9 9 7))
dims: ((1 3) (12 14))
2d result: ((1 12) (1 13) (1 14) (2 12) (2 13) (2 14) (3 12) (3 13) (3 14))
low-point (2 15)
grid: #2@1@14((5 4 5) (6 6 6) (7 9 8))
dims: ((1 3) (14 16))
2d result: ((1 14) (1 15) (1 16) (2 14) (2 15) (2 16) (3 14) (3 15) (3 16))
next point found: (2 14)
next point found: (2 16)

low-point (2 14)
grid: #2@1@13((7 5 4) (8 6 6) (9 7 9))
dims: ((1 3) (13 15))
2d result: ((1 13) (1 14) (1 15) (2 13) (2 14) (2 15) (3 13) (3 14) (3 15))

!#

'(
  (((0 0)) (((1 0)) (()))) ;; the original low point (0 1) is not there why?
  
  (((0 8) (1 9)) (((0 7) (1 8)) (((0 6)) (((0 5) (1 6)) (()) (()))) (())) (((1 8) (2 9)) (()) (()))) ;; (1 8) appears three times, why?

  (((1 2) (2 1) (2 3) (3 2)) (()) (()) (((1 3) (2 4) (3 3)) (((1 2) (1 4)) (()) (())) (((1 4) (2 5) (3 4)) (()) (()) (())) (((3 4)) (()))) (((3 1) (3 3)) (((2 1) (3 0) (4 1)) (()) (()) (())) (((3 4)) (())))) ;; (1 2) (2 1) (1 4) (3 4) (3 3) repeats

  (((3 6) (4 5) (4 7)) (((3 7)) (((2 7) (3 8)) (()) (())))(()) (((3 7) (4 8)) (((2 7) (3 8)) (()) (())) (((3 8) (4 9)) (()) (()))))) ;; (3 7) (2 7) (3 8) repeats, low point (4 6) is not there?


#!/usr/bin/env sh
exec guile -e '(@ (day09) main)' -s "$0" "$@"
!#

#!

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


scheme@(day09)> ,tr (main '())
trace: |  (main ())
trace: |  |  (parse-input)
trace: |  |  (call-with-input-file "input.txt" #<procedure 16e9a28 at <unknown port>:150:4 (p)>)
trace: |  |  |  (open-input-file "input.txt" #:binary #f #:encoding #f #:guess-encoding #f)
trace: |  |  |  (open-file "input.txt" "r" #:encoding #f #:guess-encoding #f)
trace: |  |  |  #<input: input.txt 13>
trace: |  |  |  (_ #<input: input.txt 13>)
trace: |  |  |  |  (replace #<directory (day09) 7f17e53d4960> map #<interface (guile) 7f17e8364dc0> #<procedure map (f l) | (f l1 l2) | (f …> …)
trace: |  |  |  |  |  (_ #<interface (guile) 7f17e8364dc0>)
trace: |  |  |  |  |  #<hash-table 7f17e83a23e0 0/31>
trace: |  |  |  |  |  (hashq-ref #<hash-table 7f17e83a23e0 0/31> map)
trace: |  |  |  |  |  #f
trace: |  |  |  |  |  (_ #<interface (srfi srfi-1) 7f17e83c3320>)
trace: |  |  |  |  |  #<hash-table 7f17e83e50c0 9/31>
trace: |  |  |  |  |  (hashq-ref #<hash-table 7f17e83e50c0 9/31> map)
trace: |  |  |  |  |  #t
trace: |  |  |  |  |  (module-variable #<interface (guile) 7f17e8364dc0> map)
trace: |  |  |  |  |  #<variable 7f17e838c7c0 value: #<procedure map (f l) | (f l1 l2) | (f l1 . rest)>>
trace: |  |  |  |  |  (module-variable #<interface (srfi srfi-1) 7f17e83c3320> map)
trace: |  |  |  |  |  #<variable 7f17e83eba20 value: #<procedure map (f l) | (f l1 l2) | (f l1 . rest)>>
trace: |  |  |  |  #<variable 7f17e83eba20 value: #<procedure map (f l) | (f l1 l2) | (f l1 . rest)>>
trace: |  |  |  |  (read-line #<input: input.txt 13>)
trace: |  |  |  |  |  (%read-line #<input: input.txt 13>)
trace: |  |  |  |  |  ("9976786789439313678999989767678999865435679398654323678999987654313468954569865334568916987643236789" . #\newline)
|


,profile or ,pr - profile expression



scheme@(day09)> ,pr (main '())        OR
scheme@(day09)> ,pr (main '()) #:count-calls? #t       OR
scheme@(day09)> ,tr (main '())

Part 1: 539


Part 2: 736920
%     cumulative   self             
time   seconds     seconds  procedure
 67.12      1.06      0.81  srfi/srfi-1.scm:734:0:find-tail
 10.96      0.13      0.13  srfi/srfi-1.scm:951:15
  9.59      0.12      0.12  equal?
  5.48      0.07      0.07  %after-gc-thunk
  1.37      0.03      0.02  concatenate
  1.37      0.03      0.02  list
  1.37      0.02      0.02  anon #x239b51c
  1.37      0.02      0.02  make-shared-array
  1.37      0.02      0.02  <current input>:221:31
  0.00    185.57      0.00  <current input>:277:27:recurse-basins
  0.00      1.50      0.00  srfi/srfi-1.scm:584:5:map1
  0.00      1.21      0.00  <current input>:301:0:main
  0.00      1.15      0.00  <current input>:252:0:filter-neighbouring-points
  0.00      1.06      0.00  <current input>:236:4
  0.00      0.07      0.00  anon #x239b590
  0.00      0.05      0.00  <current input>:191:0:adjacent-grid
  0.00      0.05      0.00  filter
  0.00      0.05      0.00  <current input>:156:0:low-point?
  0.00      0.03      0.00  make-shared-array
  0.00      0.02      0.00  list->array
  0.00      0.02      0.00  <current input>:212:0:make-2d-coords
  0.00      0.02      0.00  array->list
---
Sample count: 73
Total time: 1.213223684 seconds (0.155471916 seconds in GC)
scheme@(day09)>

------------------------------------


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
  ;;#:use-module (ice-9 futures)
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

(define (make-sub-grid world)
  "Returns a function that takes a slice of the large array."
  ;; list is just a pass-through so we don't change
  ;; the coordinate system.
  (cut make-shared-array world list <> <>))

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
         (grid-pair (adjacent-grid world coords)) ;; mask and subarray as pair
         (sub (cdr grid-pair))
	 (sub-zero-array (zero-array-origin sub)) ;; reset array bound to start at (0,0)
         (c-mask (car grid-pair))
         ;; this is the reason we have to 0-anchor our arrays to use our 0-anchored masks
         (c (array-ref sub-zero-array (centred-mask-i c-mask) (centred-mask-j c-mask)))) ;; value @ centre
    ;;(format #t "~%world coords: (~a, ~a)" i j)
    ;;(format #t "~%grid: ~a" (centred-mask-grid c-mask))
    ;;(format #t "~%i: ~a" (centred-mask-i c-mask))
    ;;(format #t "~%j: ~a"  (centred-mask-j c-mask))
    ;;(format #t "~%centre: ~a" c)
    (array-for-each (lambda (sub-v mask-v)
                      ;;(format #t "~%sub-v: ~a mask-v: ~a" sub-v mask-v)
                      (when mask-v (set! result (and result (< c sub-v))))) ;; the key line!
                    sub-zero-array (centred-mask-grid c-mask))
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
(define (adjacent-grid world coords)
  "Return grid centered on (i j)."
  (let* ((i (car coords))
         (j (cadr coords))
         (dims (array-dimensions world))
         (cols (1- (cadr dims)))
         (rows (1- (car dims)))
         (sub-grid (make-sub-grid world)))
    ;;(format #t "~%cols: ~a rows: ~a" cols rows)
    (cond ;; order is important!
     ((and (< (1- i) 0) (< (1- j) 0)) `(,top-left . ,(sub-grid (list+inc i) (list+inc j))))
     ((and (< (1- i) 0) (> (1+ j) cols)) `(,top-right . ,(sub-grid (list+inc i) (list+dec j))))
     ((and (> (1+ i) rows) (< (1- j) 0)) `(,bottom-left . ,(sub-grid (list+dec i) (list+inc j))))
     ((and (> (1+ i) rows) (> (1+ j) cols)) `(,bottom-right . ,(sub-grid (list+dec i) (list+dec j))))
     ((< (1- i) 0) `(,top-row . ,(sub-grid (list+inc i) (list+-inc j))))
     ((> (1+ i) rows) `(,bottom-row . ,(sub-grid (list+dec i) (list+-inc j))))
     ((< (1- j) 0) `(,left-col . ,(sub-grid (list+-inc i) (list+inc j))))
     ((> (1+ j) cols) `(,right-col . ,(sub-grid (list+-inc i) (list+dec j))))
     ;; elsewhere, not on an edge
     (else `(,other . ,(sub-grid (list+-inc i) (list+-inc j)))))))

(define (make-2d-coords arr)
  "Create a set of coordinate pairs for the given array."
  (let* ((dims (array-shape arr))
	 ;; get bounds in form iota likes
         (col-bounds (list (- (cadar dims) -1 (caar dims)) (caar dims)))
         (row-bounds (list (- (cadadr dims) -1 (caadr dims)) (caadr dims))))
    ;;(format #t "~%dims: ~a" dims)
    (let ((result (concatenate
                   (map (lambda (col)
                          (map (lambda (row) (list col row))
                               (apply iota row-bounds)))
                        (apply iota col-bounds)))))
      ;;(format #t "~%2d result: ~a" result)
      result)))

(define (make-make-upstream?)
  "Make a function that makes an upstream check function.
   The outer make allows for us to carry around the state of
   previously traversed centres in case the doubly recursive
   algorithm re-adds centre points we've already traversed - causing deadlocks.
   This can happen if 2 contiguous points have the same height for example,
   Each one will continually re-add the other as being upstream of it.
   The second make carries the state local to a specific centre point."
  (let ((traversed-coords '()))
    (lambda (centre-coord centre) ;; make-upstream?
      ;;(format #t "~%centre: ~a" centre)
      (if (member centre-coord traversed-coords)
	  (lambda (e m c) #f) ;; to avoid deadlocks return false for all input if previously traversed
	  (begin 
	    (set! traversed-coords (cons centre-coord traversed-coords))
	    (lambda (element mask-element coord) ;; upstream?
	      ;;(format #t "~%traversed-coords: ~a" traversed-coords)
	      (let ((result (cond
			     ((eqv? element 9) #f)
			     ((< element centre) #f) ;; if the candidate is less than the centre, it must be downstream
			     ((not mask-element) #f) ;; ignore diagonals
			     (else coord))))
		result)))))))


(define (filter-neighbouring-points world low-point make-upstream?)
  "Get surrounding points and mask of valid points.
   Find the value at the centre.
   Return a filtered list of all the points upstream of the centre point."
  ;;(format #t "~%low-point ~a" low-point)
  (let* ((adjacent (adjacent-grid world low-point))
         (mask (car adjacent))
         (grid (cdr adjacent))
         (c (array-ref grid (car low-point) (cadr low-point)))
         (upstream? (make-upstream? low-point c))) ;; c is value @ centre
    ;;(format #t "~%grid: ~a" grid)
    ;; Get a list of the grid coordinates centred on c
    ;; Squash both the grid and the mask value into a 1D list representation
    ;; kick out any coordinates that are not upstream from the low point,
    ;; returning the remaining coordinates as a single list.
    (filter-map upstream?
                (concatenate (array->list grid))
                (concatenate (array->list (centred-mask-grid mask)))
                (make-2d-coords grid))))

(define (make-recurse-basins make-upstream?)
  "Create a function that will doubly-recursively walk
   each low-point and any points upstream from those low-points.
   We pass in a function to create an upstream check that is seeded
   with a list of previously walked points."
  (letrec ((recurse-basins (lambda (world low-points)
			     (if (null? low-points)
				 '() ;; base case start with an empty list
				 (let ((coords (filter-neighbouring-points world (car low-points) make-upstream?)))
				   ;; doubly recursive - cons all the coordinates in the neighbourhood of the
				   ;; low point (dictated by masks) that are upstream of the low-point, with any
				   ;; points that are then upstream from these new neighbourhood points just generated, and
				   ;; cons all this to the results of all the other initial low-points.
				   (cons (cons coords (recurse-basins world coords))
					 (recurse-basins world (cdr low-points))))))))
    recurse-basins))

(define (flatten lists)
  "Mine a list of lists of any depth, and return a single list of all
   numerical pairs (coords) within the lists."
  (cond
   ((null? lists) '())
   ((and (pair? lists)
	 (pair? (car lists))
	 (number? (caar lists))) lists)
   ((pair? lists) (append (flatten (car lists))
			  (flatten (cdr lists))))
   (else (error "impossible!"))))

(define (main args)
  (let* ((world (parse-input "input.txt"))
         (world-coord-pairs (make-2d-coords world)))
    ;;(format #t "~%world: ~a~%" world)
    ;;(format #t "~%world array dimensions: ~a~%" world-coord-pairs)
    (let ((low-points (filter (cut low-point? world <>)
                              world-coord-pairs)))
      (format #t "~%~%Part 1: ~a~%"
              (fold (lambda (p sum) ;; sum all the "heights+1"
                      (+ sum 1 (array-ref world (car p) (cadr p))))
                    0
                    low-points))

      ;; embed each low-point in a list and "map append" so each low-point is
      ;; added to it's corresponding list of basin points returned by recurse-basins.
      ;; the make-make-upstream? creates a make-upstream? function that carries 
      ;; an initially empty list as a closure to record all points walked within the world.
      ;; We can use a single closure as each point can only ever exist in one basin.
      ;; The resulting structure is an n-deep list of coords representing each
      ;; point within a low-point's basin.  So we flatten each entry and calc the
      ;; number of unique points in each basin (length).
      ;; Delete duplicates is still required despite recording of all points walked,
      ;; because the appending on the low-points list at the end may produce duplicates (I think).
      ;; Finally sort the list take the 3 largest, and multiply them together.
      (format #t "~%~%Part 2: ~a~%" ((compose (cut apply * <>) (cut take <> 3) (cut sort <> >))
				     (map (compose length delete-duplicates flatten)
					  (let ((rb (make-recurse-basins (make-make-upstream?))))
					    (map append (rb world low-points)
						 (map list low-points)))))))))





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


!#

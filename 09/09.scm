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
(define (make-sub-grid world)
  "Returns a function that takes a slice of the large array."
  (lambda (i-bounds j-bounds)
    ;; list is just a pass-through so we don't change
    ;; the coordinate system.  The idea is to use
    ;; array-for-each or similar to traverse the array
    ;; without coordinates.
    ;; The trick will be filtering out all the diagonals!
    ;;(format #t "~%i bounds: ~a j bounds: ~a" i-bounds j-bounds)
    (make-shared-array world list i-bounds j-bounds)))


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
   algorithm re-adds points we've already traversed - causing deadlocks.
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
 

(define (process world low-point make-upstream?)
  ;;(format #t "~%low-point ~a" low-point)
  (let* ((adjacent (adjacent-grid world low-point)) ;; make zero-array calls all local like below
         (mask (car adjacent))
         (grid (cdr adjacent))
         (c (array-ref (zero-array-origin grid) (centred-mask-i mask) (centred-mask-j mask)))
         (upstream? (make-upstream? low-point c))) ;; c is value @ centre
    ;;(format #t "~%grid: ~a" grid)
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
				 '()
				 (let ((result (process world (car low-points) make-upstream?)))
				   (cons (cons result (recurse-basins world result))
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

      ;; embed each low-point in a list
      ;; append this to a list of upstream points within the low-point's basin
      ;; the make-make-upstream? creates a make-upstream? function carries a
      ;; an initially empty list as closure to record all points walked within the world.
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

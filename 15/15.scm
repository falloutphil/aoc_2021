#!/usr/bin/env sh
# -*- mode:scheme; geiser-scheme-implemenatation: guile; mode:company -*-
exec guile -e '(@ (day15) main)' -s "$0" "$@"
!#

;; Basically do this: https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm

#!

Node:
distance: #f

Set to 0 now inspect all neighbours.


1. Mark all nodes unvisited. Create a set of all the unvisited nodes called the unvisited set.

2. Assign to every node a tentative distance value: set it to zero for our initial node and to infinity for all other nodes. The tentative distance of a node v is the length of the shortest path discovered so far between the node v and the starting node. Since initially no path is known to any other vertex than the source itself (which is a path of length zero), all other tentative distances are initially set to infinity. Set the initial node as current.

3. For the current node, consider all of its unvisited neighbors and calculate their tentative distances through the current node. Compare the newly calculated tentative distance to the current assigned value and assign the smaller one. For example, if the current node A is marked with a distance of 6, and the edge connecting it with a neighbor B has length 2, then the distance to B through A will be 6 + 2 = 8. If B was previously marked with a distance greater than 8 then change it to 8. Otherwise, the current value will be kept.

4. When we are done considering all of the unvisited neighbors of the current node, mark the current node as visited and remove it from the unvisited set. A visited node will never be checked again.

5. If the destination node has been marked visited (when planning a route between two specific nodes) or if the smallest tentative distance among the nodes in the unvisited set is infinity (when planning a complete traversal occurs when there is no connection between the initial node and remaining unvisited nodes), then stop. The algorithm has finished.

6. Otherwise, select the unvisited node that is marked with the smallest tentative distance, set it as the new current node, and go back to step 3.
!#

;; The problem with this is that it gets stuck because when it heads a dead-end, it can't fallback
;; to a previous path that was discounted in favour of the "best" choice at that time.
;; Thus if the best choice leads to a point where there are no more unvisted neighbours, but
;; we haven't have found the solution then it should find a more expensive neighbour from some previous
;; iteration and then exhaust that, if that fails, it tries the next expensive neighbour, and so on.

;; Here instead we always assume the next vistor can be found from the current neighbours.
;; Thus rather than replace the next visitor each time we find an improvement, this
;; needs to a be a list like structure of potential candidate, and cost to that point.
;; This list is then sorted to find the cheapest next step, which may be to go back
;; and continue on a different path, if subsequent searches prove expensive on the current path.

;; Note this is the clever thing - if won't exhaust a single path, it will forever consider every path
;; up until now and advance the least expensive path so far, even if it is not the closest node
;; to the target.

(define-module (day15)
  #:export (main)
  #:use-module (ice-9 rdelim)  ;; read-line
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)   ;; first, second, last, list-copy
  #:use-module (srfi srfi-11)  ;; let-values
  #:use-module (srfi srfi-42)) ;; list-ec/eager comprehensions

(define (parse-input filename)
  "Read file of numbers into 2D array."
  (call-with-input-file filename
    (lambda (p)
      (list->array 2 (list-ec (:port line p read-line)
                              (list-ec (:string ch line)
                                       (- (char->integer ch) 48)))))))

(define (make-2d-coords arr)
  "Create a set of coordinate pairs for the given array."
  (let ((d (array-dimensions arr)))
    (match d
      ((max-i max-j) (list-ec (: i max-i) (: j max-j) (list i j))))))

(define (make-neighbour-coords arr)
  (?? (i j)
    (filter (?? (coords)
	      (apply array-in-bounds? (cons arr coords)))
	    (list (list (1- i) j)
		  (list (1+ i) j)
		  (list i (1- j))
		  (list i (1+ j))))))


(define (get-infinity cost-array)
  "Max cost for the current input + 1 can
   be used as infinity."
  (let ([infinity 0])
    (array-for-each (?? (i) (set! infinity (+ infinity i))) cost-array)
    (1+ infinity)))

(define (make-route-array cost-array infinity)
  (let ([ra (apply make-array (cons infinity (array-dimensions cost-array)))])
    (array-set! ra 0 0 0)
    ra))

;; to visit array - start with all true
(define (make-visited-array cost-array)
  (apply make-array (cons #t (array-dimensions cost-array))))


;; you need 2 arrays or an array of objects
;; you have the cost of each vertex in the array
;; but you also need to track the cost so far of
;; each node (starting with infinity).
(define (update-neighbours current-visit neighbours route-array cost-array visited-array)
  (let ([current-cost (apply array-ref (cons route-array current-visit))]
	[next-visit #f])
      (map (?? (coord)
	     (let* ([old-cost (apply array-ref (cons route-array coord))]
		    [new-cost (+ current-cost (apply array-ref (cons cost-array coord)))]
		    [the-cost (min new-cost old-cost)])
	       (when (and (apply array-ref (cons visited-array coord))
			  (or (not next-visit) (< the-cost
						  (apply array-ref (cons route-array next-visit)))))
		 (apply array-set! (append (list route-array the-cost) coord))
		 (set! next-visit coord))))
	   neighbours)
      ;; only visit once
      (apply array-set! (append (list visited-array #f) current-visit))
    next-visit))


(define (main args)
  (let* ([cost-array (parse-input "test_input.txt")]
	 [infinity (get-infinity cost-array)] ;; factor this out
	 [end-point (map 1- (array-dimensions cost-array))] 
	 [route-array (make-route-array cost-array infinity)]
	 [visited-array (make-visited-array cost-array)]
	 [get-neighbours (make-neighbour-coords route-array)])
    (format #t "~%cost-array: ~a~%route-array: ~a" cost-array route-array)

    ;; currently gets stuck in a corner with nowhere left to vist:
    ;; neighbours: ()
    ;; next visit: #f
    ;; Also the route of the algo looks odd from the backtrace.
    ;; current cost is the cumulative cost to date which for the test
    ;; should go: 0 1 2 1 3 6 5 1 1
    ;; cumulative: 0 1 3 4 7 13 18 19 20
    (let loop ([current-visit '(0 0)])
      (let ([current-cost (apply array-ref (cons route-array current-visit))])
	(format #t "~%~%current vist: ~a" current-visit)
	(format #t "~%current cost: ~a" current-cost)
      (unless (or (equal? current-visit end-point)
		  (> current-cost infinity))
	(let* ([neighbours (apply get-neighbours current-visit)]
	       ;; update route-array for each neighbour using cost-array
	       [next-visit (update-neighbours current-visit neighbours
					      route-array cost-array visited-array)])
	  (format #t "~%neighbours: ~a" neighbours)
	  (format #t "~%next visit: ~a" next-visit) 
	  (format #t "~%cost-array: ~a~%route-array: ~a~%visited array: ~a" cost-array route-array visited-array)
	  (loop next-visit))))
      (format #t "~%~%Result : ~a~%"
	      (apply array-ref (cons route-array current-visit))))))


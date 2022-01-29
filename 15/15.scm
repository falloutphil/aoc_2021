#!/usr/bin/env sh
# -*- mode:scheme; geiser-scheme-implemenatation: guile -*-
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
  (λ (i j)
    (filter (λ (coords)
	      (apply array-in-bounds? (cons arr coords)))
	    (list (list (1- i) j)
		  (list (1+ i) j)
		  (list i (1- j))
		  (list i (1+ j))))))

;; you need 2 arrays or an array of objects
;; you have the cost of each vertex in the array
;; but you also need to track the cost so far of
;; each node (starting with infinity).
(define (update-neighbours cost-array get-neighbours coord)
  (let ([neighbours (get-neighbours coord)])
    (format #t "~%neighbours: ~a" neighbours)))

(define (main args)
  (let* ([cost-array (parse-input "test_input.txt")]
	 [get-neighbours (make-neighbour-coords cost-array)])
    (format #t "~%cost-array: ~a" cost-array)))


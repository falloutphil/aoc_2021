#!/usr/bin/env sh
# -*- mode:scheme; geiser-scheme-implemenatation: guile -*-
exec guile -e '(@ (day12) main)' -s "$0" "$@"
!#

(define-module (day12)
  #:export (main)
  #:use-module (ice-9 rdelim) ;; read-line
  #:use-module (ice-9 match)
  #:use-module (ice-9 hash-table)
  #:use-module (srfi srfi-1) ;; concatenate
  #:use-module (srfi srfi-42)) ;; list-ec/eager comprehensions

#!
Construct a dictionary of nodes
Where the values are a list of onwards nodes

connect = like a defaultdict(list)

Split each pair into the list
(he DX)
reverse the pair
(DX he)

Zip them together
((he DX) (DX he))

for he, DX
if DX != 'start' or he != 'end'
connect[he].append(DX)

for DX, he
if he != 'start' or DX != 'end' 
connect[DX].append(he)

THEN

new list "path" init with element "start"

then loop
look up last path entry in dictionary
if it is upper (big cave), or not already in the path (small cave)
if it is end return count += 1
(recurse data, path ++ point)

!#

(define (file->list filename)
  "Read file, split on '-' and add list and reverse list to list.
   Then flatten reversed groups and filter out lists that
   end with start, or start with end."
  (filter (λ (node-pair)
	    (match node-pair
	      ((n1 n2) (and (not (or (string=? n2 "start")
				     (string=? n1 "end")))))))
	  (concatenate
	   (call-with-input-file filename
	     (λ (p)
	       (list-ec (:port line p read-line)
			(let ((nodes (string-split line #\-)))
			  (list nodes (reverse nodes)))))))))

(define (display-hash hm)
  "Debug function to look at dictionary."
  (hash-for-each
   (λ (k v) (format #t "~%(~a, ~a)" k v))
   hm))

(define (build-connection-dict lst)
  "Create a dictionary of (nodes, list of next nodes)."
  (let ((cd (make-hash-table)))
    (for-each
     (λ (node-pair)
       (match node-pair
	 ((n1 n2) (hash-set! cd n1
			     (cons n2 (hash-ref cd n1 '()))))))
     lst)
    cd))

(define (make-find-paths part2)
  "Return fn what will recurse through each and 
   every path from start to end, and count them."
  (let ((count 0))
    (λ (cd)
      (let recurse ((paths '("start"))
		    (part2 part2)) ;; each *new* recursion can visit a small cave twice
	(if part2
	    (begin ;; part 2 - will reuse part 1, it could be more pretty!
	      (for-each ;; for each next node from current
	       (λ (node)
		 (if (string=? node "end") 
		     (set! count (1+ count)) ;; count the newly completed route
		       (if (and (string-every char-lower-case? node)
				  (member node paths))
			   (recurse (cons node paths) #f) ;; we have been the small cave once, revert to part 1 logic
			   (recurse (cons node paths) #t)))) ;; or add node to visited paths and recurse
	       (hash-ref cd (car paths)))) ;; next moves from head of path - these will have been added by the recurse
	    (begin ;; part 1
	      (for-each ;; for each next node from current
	       (λ (node)
		 ;; check the next node is a valid move
		 (if (or (string-every char-upper-case? node) ;; large cave shortcircuit
			 (not (member node paths))) ;; or small cave if not already in the path
		     (if (string=? node "end") 
			 (set! count (1+ count)) ;; count the newly completed route
			 (recurse (cons node paths) #f)))) ;; or add node to visited paths and recurse
	       (hash-ref cd (car paths))))) ;; next moves from head of path - these will have been added by the recurse
	    count)))) ;; return the final count


(define (main args)
  (let* ((input-list (file->list "input.txt"))
	 (dict (build-connection-dict input-list))
	 (find-paths-1 (make-find-paths #f))
	 (find-paths-2 (make-find-paths #t)))
    ;;(format #t "~%input: ~a~%" (file->list "test_input.txt"))
    ;;(format #t "~%dict: ~a~%" (display-hash dict))
    (format #t "~%Part 1: ~a~%" (find-paths-1 dict))
    (format #t "~%Part 2: ~a~%" (find-paths-2 dict))))

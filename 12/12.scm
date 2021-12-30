#!/usr/bin/env sh
# -*- mode:scheme; geiser-scheme-implemenatation: guile -*-
exec guile -e '(@ (day12) main)' -s "$0" "$@"
!#

(define-module (day12)
  #:export (main)
  #:use-module (oop goops) 
  #:use-module (ice-9 rdelim) ;; read-line
  #:use-module (ice-9 match)
  #:use-module (ice-9 hash-table)
  #:use-module (srfi srfi-1) ;; concatenate
  #:use-module (srfi srfi-26) ;; cut
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
if DX != 'start'
connect[he].append(DX)

for DX, he
if DX != 'start'
connect[DX].append(he)

# can't go from end to something else
del connect[end]

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
  (hash-for-each
   (λ (k v) (format #t "~%(~a, ~a)" k v))
   hm))

(define (build-connection-dict lst)
  (let ((cd (make-hash-table)))
    (for-each
     (λ (node-pair)
       (match node-pair
	 ((n1 n2) (hash-set! cd n1
			    (cons n2 (hash-ref cd n1 '()))))))
     lst)
    cd))
       
(define (main args)
  (let* ((input-list (file->list "test_input.txt"))
	 (dict (build-connection-dict input-list)))
  (format #t "~%input: ~a~%" (file->list "test_input.txt"))
  (format #t "~%dict: ~a~%" (display-hash dict))))

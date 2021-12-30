#!/usr/bin/env sh
# -*- mode:scheme; geiser-scheme-implemenatation: guile -*-
exec guile -e '(@ (day12) main)' -s "$0" "$@"
!#

(define-module (day12)
  #:export (main)
  #:use-module (oop goops) 
  #:use-module (ice-9 rdelim) ;; read-line
  #:use-module (ice-9 match)
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
if it is upper, or not already in the path
   if it is end return count += 1
   (recurse data, path ++ point)

!#

(define (main args)
  (format #t "~%hello~%"))

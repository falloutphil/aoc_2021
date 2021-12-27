#!/usr/bin/env sh
# -*- mode:scheme; geiser-scheme-implemenatation: guile -*-
exec guile -e '(@ (day11) main)' -s "$0" "$@"
!#

#!
GEISER NOTES

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
!#


#!
OCTOPUSES

Create list of list from file convert to 2d-array

10x10 grid

Then

+1 whole grid

filter points that >= 9

make subgrids around them and increment members

filter points that >=9 and continue with original grid <- doubly recursive.

THEN - Reset all flashed octopuses to 0.

Possible we'll need an array of classes, but start with int
!#


(define-module (day11)
  #:export (main)
  #:use-module (oop goops) 
  #:use-module (ice-9 rdelim) ;; read-line
  #:use-module (srfi srfi-1) ;; concatenate, filter-map, compose, append-reverse
  #:use-module (srfi srfi-26) ;; cut
  #:use-module (srfi srfi-42)) ;; list-ec

(define (parse-input filename)
  "Read file of numbers into 2D array."
  (call-with-input-file filename
    (lambda (p)
      ;; The list of values obtained by evaluating "line" once for each binding in the sequence defined by the qualifiers.
      ;; reads from p until eof
      ;; line is read through the sequence (read-line p)
      ;; line is the variable produced from repeatedly calling (read-line p) until eof?
      ;; https://practical-scheme.net/gauche/man/gauche-refe/Eager-comprehensions.html
      (list->array 2 (list-ec (:port line p read-line)
			      (list-ec (:string ch line)
				       ((compose string->number string) ch)))))))



(define (main args)
  (format #t "~%hello~%"))

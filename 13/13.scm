#!/usr/bin/env sh
# -*- mode:scheme; geiser-scheme-implemenatation: guile -*-
exec guile -e '(@ (day13) main)' -s "$0" "$@"
!#

;; https://gitlab.com/guile-syntax-parse/guile-syntax-parse
(add-to-load-path  "/home/phil/installs/guile-syntax-parse")

(define-module (day13)
  #:export (main)
  #:use-module (syntax parse)
  #:use-module (ice-9 rdelim) ;; read-line
  #:use-module (ice-9 match)
  #:use-module (ice-9 hash-table)
  #:use-module (srfi srfi-1) ;; last
  #:use-module (srfi srfi-26) ;; cut
  #:use-module (srfi srfi-42)) ;; list-ec/eager comprehensions


;; Macro to split the result of file->list
;; into a list of coords and a list of folds.
(define parse-input
  (syntax-parser
   [(coords ...+ #f folds ...+)
    #'((coords ...) (folds ...))])
  )

(define (file->list filename)
  "Read input and split into list of 
   coordinates and folds."
  (let ((lst (call-with-input-file filename
	       (λ (p)
		 (list-ec (:port line p read-line)
			  (cond
			   ((string-any (cut eqv? <> #\,) line)
			    (string-split line #\,))
			   ((string-null? line) #f) ;; blank line
			   ((string= line "fold along x=" 1 13 1 13)
			    `(x ,(last (string-split line #\=))))
			   ((string= line "fold along y=" 1 13 1 13)
			    `(y ,(last (string-split line #\=))))
			   (else (error "bad input!"))))))))
    (parse-input lst)))
      


(define (main args)
  (match-let (((coords folds) (file->list "input.txt")))
    (format #t "~%coords: ~a~%" coords)
    (format #t "~%folds: ~a~%" folds)))

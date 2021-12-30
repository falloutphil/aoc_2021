#!/usr/bin/env sh
# -*- mode:scheme; geiser-scheme-implemenatation: guile -*-
exec guile -e '(@ (day13) main)' -s "$0" "$@"
!#

(define-module (day13)
  #:export (main)
  #:use-module (ice-9 rdelim) ;; read-line
  #:use-module (ice-9 match)
  #:use-module (ice-9 hash-table)
  #:use-module (srfi srfi-1) ;; last
  #:use-module (srfi srfi-11) ;; let-values
  #:use-module (srfi srfi-26) ;; cut
  #:use-module (srfi srfi-42)) ;; list-ec/eager comprehensions

(define (file->list filename)
  (let* ((lst (call-with-input-file filename
	       (λ (p)
		 (list-ec (:port line p read-line)
			  (cond
			   ((string-any (cut eqv? <> #\,) line)
			    (string-split line #\,))
			   ((string-null? line) #f) ;; blank line
			   ((string= line "fold along x=" 1 13 1 13)
			    (last (string-split line #\=)))
			   ((string= line "fold along y=" 1 13 1 13)
			    (last (string-split line #\=)))
			   (else (error "bad input!")))))))
	 (coords (take-while identity lst)) ;; up to blank line
	 (folds (cdr (drop-while identity lst))))      
    (values coords (first folds) (second folds))))


(define (main args)
  (let-values (((coords x-fold y-fold) (file->list "test_input.txt")))
    (format #t "~%coords: ~a~%" coords)
    (format #t "~%x-fold: ~a~%" x-fold)
    (format #t "~%y-fold: ~a~%" y-fold)))

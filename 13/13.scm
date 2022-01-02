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
  #:use-module (srfi srfi-11) ;; let-values
  #:use-module (srfi srfi-26) ;; cut
  #:use-module (srfi srfi-42)) ;; list-ec/eager comprehensions


;; Macro to split the result of file->list
;; into a list of coords and a list of folds.
(define parse-input
  (syntax-parser
   [(coords ...+ #f folds ...+)
    #'((coords ...) (folds ...))])
  )

;; https://stackoverflow.com/a/70545115/2904770
(define (handle-fold line var)
  "Can be used as a cond test which returns a list
   of folds as truth (meaning no expression is needed)."
  (let ((str (string-append "fold along " (symbol->string var) "=")))
    (and (string= line str 1 13 1 13)
         (list var ((compose string->number
			     last
			     (cut string-split <> #\=))
		    line)))))

(define (file->list filename)
  "Read input and split into list of 
   coordinates and folds."
  (let ((lst (call-with-input-file filename
	       (λ (p)
		 (list-ec (:port line p read-line)
			  (cond
			   ((string-any (cut eqv? <> #\,) line)
			    (map string->number (string-split line #\,)))
			   ((string-null? line) #f) ;; blank line
			   ((handle-fold line 'x))
			   ((handle-fold line 'y))
			   (else (error "bad input!"))))))))
    (parse-input lst)))
      

(define (dimensions coords)
  (let-values ([(x y) (unzip2 coords)])
    `(,(apply max x) ,(apply max y))))

(define (main args)
  (match-let* ([(coords folds) (file->list "test_input.txt")]
	       [(max-x max-y) (dimensions coords)])
    (format #t "~%coords: ~a~%" coords)
    (format #t "~%folds: ~a~%" folds)
    (format #t "~%dims - x: ~a y: ~a~%" max-x max-y)
    (let ([paper (make-typed-array 'b #f max-y max-x)])
      (format #t "~%paper: ~a~%" paper))))


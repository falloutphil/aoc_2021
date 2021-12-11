#!/usr/bin/env sh
exec guile -e '(@ (day07) main)' -s "$0" "$@"
!#

(define-module (day07)
  #:export (main)
  #:use-module (ice-9 rdelim) ;; read-line
  #:use-module (srfi srfi-1) ;; concatenate
  #:use-module (srfi srfi-11) ;; let-values
  #:use-module (srfi srfi-43)) ;; vector-fold

(define (get-input filename)
  "Take first line of filename as input.
   Convert to list of numbers."
  (map string->number
       (string-split (call-with-input-file filename read-line)
		     #\,)))

(define (main args)
  (let ((locations (get-input "test_input.txt")))
    (format #t "~%~%part 1: ~a~%" locations)))

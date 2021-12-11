#!/usr/bin/env sh
exec guile -e '(@ (day06) main)' -s "$0" "$@"
!#

(define-module (day06)
  #:export (main)
  #:use-module (ice-9 rdelim) ;; read-line
  #:use-module (srfi srfi-1) ;; iota, last
  #:use-module (srfi srfi-26)) ;; cut

(define (get-input filename)
  "Take first line of filename as input."
  (call-with-input-file filename read-line))
  
  
(define (main args)
  (let ((input (get-input "test_input.txt")))
    (format #t "~%~%input: ~a~%" input)))

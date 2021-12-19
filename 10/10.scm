#!/usr/bin/env sh
exec guile -e '(@ (day10) main)' -s "$0" "$@"
!#

(define-module (day10)
  #:export (main)
  #:use-module (ice-9 rdelim) ;; read-line
  #:use-module (srfi srfi-1) ;; concatenate
  #:use-module (srfi srfi-11) ;; let-values
  #:use-module (srfi srfi-26) ;; cut
  #:use-module (srfi srfi-42)) ;; list-ec

(define (main args)
  (format #t "~%hello~%"))

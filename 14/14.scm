#!/usr/bin/env sh
# -*- mode:scheme; geiser-scheme-implemenatation: guile -*-
exec guile -e '(@ (day14) main)' -s "$0" "$@"
!#

(define-module (day14)
  #:export (main)
  #:use-module (ice-9 rdelim)  ;; read-line
  #:use-module (ice-9 match) 
  #:use-module (srfi srfi-1)   ;; last
  #:use-module (srfi srfi-11)  ;; let-values
  #:use-module (srfi srfi-26)  ;; cut
  #:use-module (srfi srfi-42)) ;; list-ec/eager comprehensions


(define (main args)
  (format #t "~%Hello~%"))

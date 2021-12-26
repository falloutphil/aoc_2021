#!/usr/bin/env sh
# -*- mode:scheme; geiser-scheme-implemenatation: guile -*-
exec guile -e '(@ (day11) main)' -s "$0" "$@"
!#

(define-module (day11)
  #:export (main)
  #:use-module (oop goops) 
  #:use-module (ice-9 rdelim) ;; read-line
  #:use-module (srfi srfi-1) ;; concatenate, filter-map, compose, append-reverse
  #:use-module (srfi srfi-26) ;; cut
  #:use-module (srfi srfi-42)) ;; list-ec

(define (main args)
  (format #t "~%hello~%"))

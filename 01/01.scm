#!/usr/bin/env sh
exec guile -e '(@ (day01) main)' -s "$0" "$@"
!#

(define-module (day01)
  #:export (main)
  #:use-module (ice-9 rdelim) ;; read-line
  #:use-module (srfi srfi-42)) ;; list-ec

(define (file->list filename transform)
  (call-with-input-file filename
    (lambda (p)
      (map transform (list-ec (:port line p read-line) line)))))

;; change transform for function that actually returns the result!
;; change to macro with nicer syntax
(define (main args)
  (format #t
	  "~%Result: ~a~%"
	  (file->list "input.txt" string->number)))

#!/usr/bin/env sh
exec guile -e '(@ (day08) main)' -s "$0" "$@"
!#

(define-module (day08)
  #:export (main)
  #:use-module (ice-9 rdelim) ;; read-line
  #:use-module (srfi srfi-1) ;; concatenate, count
  #:use-module (srfi srfi-11) ;; let-values
  #:use-module (srfi srfi-26) ;; cut
  #:use-module (srfi srfi-42)) ;; list-ec

(define (split-digits segment-str)
  "Clean ends, split on whitespace into digits as segments"
  ;;(format #t "~%~%split-digits: ~a~%" segment-str)
  (string-split (string-trim-both segment-str) #\ ))

(define (file->digit-inputs filename)
  "Read filename, and split each line at the pipe.
   Then split data either side of the pipe into lists of segments."
  (call-with-input-file filename
    (lambda (p)
      (map (compose (lambda (str-lst)
		      ;;(format #t "~%~%str-lst: ~a~%" str-lst)
		      ;; make pairs of ((digits message) (digits message) ...)
		      (list (split-digits (car str-lst))
			    (split-digits (cadr str-lst))))
                    (cut string-split <> #\|))
           (list-ec (:port line p read-line) line)))))

#!
digit vs segments - 1 4 7 8
0 6
1 2 *
2 5
3 5
4 4 *
5 5 
6 6
7 3 *
8 7 *
9 6

Find via count of (2 4 3 7)

!#

(define (count-cipher-segments segments)
  (let ((cipher-lst (cadr segments)))
    (map string-length cipher-lst))) 
	
(define (main args)
  (let* ((di (file->digit-inputs "input.txt"))
	 (ciphertext-segment-counts (concatenate (map count-cipher-segments di))))
    ;;(format #t "~%~%Input: ~a~%" ciphertext-segment-counts)
    (format #t "~%~%Part 1: ~a~%" (count (lambda (n)
					       (case n ((2 4 3 7) #t) (else #f)))
					     ciphertext-segment-counts))))

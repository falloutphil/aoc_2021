#!/usr/bin/env sh
exec guile -e '(@ (day03) main)' -s "$0" "$@"
!#

(define-module (day03)
  #:export (main)
  #:use-module (ice-9 rdelim) ;; read-line
  #:use-module (srfi srfi-26) ;; cut
  #:use-module (srfi srfi-42)) ;; list-ec

;; This will one day be replaced with:
;; https://srfi.schemers.org/srfi-178/srfi-178.html
(define (list->bitlist lst)
  "Convert all 0s to #f as Guile only recognises #f when converting to bitvectors"
  (cond
   ((null? lst) '())
   ((char=? (car lst) #\0) (cons #f (list->bitlist (cdr lst))))
   (else (cons (car lst) (list->bitlist (cdr lst))))))
  
(define (file->bitlist filename)
  "Read filename, and convert each line to a list.
   Then replace all 0s with #f.
   Then convert to bitvector."
  (call-with-input-file filename
    (lambda (p)
      (map (compose list->bitlist string->list)
           (list-ec (:port line p read-line) line)))))

(define (main args)
  (let* ((b-array (list->array 2 (file->bitlist "input.txt")))
         (t-barray (transpose-array b-array 1 0))
         (t-bvec (map list->bitvector (array->list t-barray)))
         (gamma (list->bitvector (map (compose (cut < <> 500) bitvector-count) t-bvec)))
         (epsilon (list->bitvector (map (compose (cut > <> 500) bitvector-count) t-bvec))))
    (format #t "~%Part One Result: ~a~%" t-bvec)
    (format #t "~%Part One Result: ~a~%" gamma)
    (format #t "~%Part One Result: ~a~%" epsilon)))

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

(define (bitlist->list lst)
  "Convert all #f to 0 and #t to 1"
  (cond
   ((null? lst) '())
   ((car lst) (cons 1 (bitlist->list (cdr lst))))
   (else (cons 0 (bitlist->list (cdr lst))))))

;; We don't convert to a bitvector here because list->array
;; only handles lists of lists not lists of bitvectors.
(define (file->bitlist filename)
  "Read filename, and convert each line to a list.
   Then replace all 0s with #f."
  (call-with-input-file filename
    (lambda (p)
      (map (compose list->bitlist string->list)
           (list-ec (:port line p read-line) line)))))

(define (binary->decimal binary-list)
  "Calculate decimal equivalent of list of binary"
  (let loop ((rev-bl (reverse binary-list)))
    (if (null? rev-bl)
        0 
        (+ (car rev-bl) (* 2 (loop (cdr rev-bl)))))))

;; Using bitvectors turned out to be a pain because
;; 0 is not accepted as #f, and converting back 1 becomes #t
;; Also bitvectors don't have a simple conversion to and from
;; binary numbers.
(define (main args)
  (let* ((one-false-array (list->array 2 (file->bitlist "input.txt"))) ; get the input convert to 2D array of 1s and #fs
         (t-one-false-array (transpose-array one-false-array 1 0)) 
         (t-bvec (map list->bitvector (array->list t-one-false-array))) ; convert transposed columns to bitvectors
         ;; the question doesn't specify output when each bit is equally as common so ignore that case!
         (gamma-list (map (compose (cut > <> 500) bitvector-count) t-bvec)) ; count the 1s for transposed column return #t for each >500
         (gamma (binary->decimal (bitlist->list gamma-list))) ; convert the list into integers then decimal
         (epsilon (logxor gamma #b111111111111))) ; flip the bits to get epsilon easily
    (format #t "~%Part One: ~a~%" (* gamma epsilon))))

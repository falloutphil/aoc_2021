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

(define (all-indices lst x)
  "Return all indices that match x in list."
  (let loop ((lst lst)
             (n 0))
    (cond
     ((null? lst) '())
     ((eqv? (car lst) x) (cons n (loop (cdr lst) (+ n 1))))
     (else (loop (cdr lst) (+ n 1))))))

;; Test input data from the example in the question.
(define test-report
  #2((#f #f #\1 #f #f)
    (#\1 #\1 #\1 #\1 #f)
    (#\1 #f #\1 #\1 #f)
    (#\1 #f #\1 #\1 #\1)
    (#\1 #f #\1 #f #\1)
    (#f #\1 #\1 #\1 #\1)
    (#f #f #\1 #\1 #\1)
    (#\1 #\1 #\1 #f #f)
    (#\1 #f #f #f #f)
    (#\1 #\1 #f #f #\1)
    (#f #f #f #\1 #f)
    (#f #\1 #f #\1 #f)))

(define (rating-filter bitarray n cmp)
  "Slice column and determine dominant bit in slice.
   Work out indices with the dominant bit (using cmp to compare).
   Create a new array with row numbers matching the indices.
   Repeat until one result left."
  (let* ((lh-slice (array->list
                    (make-shared-array bitarray
                                       (lambda (i) (list i n)) ;; take slice holding column constant as n
                                       `(0 ,(- (array-length bitarray) 1))))) ;; bound by length of array
         (bv-slice (list->bitvector lh-slice)) ;; convert to bitvector
         (dominant-bit (if (cmp (bitvector-count bv-slice) (/ (bitvector-length bv-slice) 2)) #\1 #f)) ;; count 1s and 0s - decide which is dominant
         (indices (all-indices lh-slice dominant-bit)) ;; get indices
         (new-array (list->array 2 (map (compose array->list (cut array-cell-ref bitarray <>)) indices)))) ;; contains only indices
    (if (> (array-length new-array) 1)
        (rating-filter new-array (+ n 1) cmp)
        ((compose binary->decimal bitlist->list vector->list) (array-cell-ref new-array 0)))))   


;; Using bitvectors turned out to be a pain because
;; 0 is not accepted as #f, and converting back 1 becomes #t
;; Also bitvectors don't have a simple conversion to and from
;; binary numbers.
(define (main args)
  (let* ((one-false-array (list->array 2 (file->bitlist "input.txt"))) ; get the input convert to 2D array of 1s and #fs
         (t-one-false-array (transpose-array one-false-array 1 0)) 
         (t-bvec (map list->bitvector (array->list t-one-false-array))) ; convert transposed columns to list of bitvectors
         ;; the question doesn't specify output when each bit is equally as common so ignore that case!
         (gamma-list (map (compose (cut > <> 500) bitvector-count) t-bvec)) ; count the 1s for transposed column return #t for each >500
         (gamma (binary->decimal (bitlist->list gamma-list))) ; convert the list into integers then decimal
         (epsilon (logxor gamma #b111111111111))) ; flip the bits to get epsilon easily
    (format #t "~%Part One: ~a~%" (* gamma epsilon)))
  
  (let* ((one-false-array (list->array 2 (file->bitlist "input.txt"))) ; get the input convert to 2D array of 1s and #fs
         (oxygen (rating-filter one-false-array 0 >=))
         (co2 (rating-filter one-false-array 0 <)))
    (format #t "~%Part Two: ~a~%" (* oxygen co2))))

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
Part 1:

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

Part 2:

Start with 1,4,7,8
Found a
Then 0,9, and 6 must be 8-1 count = 7-1 segments = 6.  Giving d,e,c
Which one is not in 1 is 6, giving c (and f from 1)
Which one is not in 4 is e, the other is d.
Found d,e - I know 0,1,4,7,8,9
Now 4-1-d is b.
Found b
Then 2,3,5 all have 5 segs - only one contains b, that 5.
3 is one different from 5 - that is c
The other one is 2

a,b,c,d,e
0,1,2,3,4,5,7,8,9

Take 1 - remove c, give you f

Remaining position must be g

DONE!

a -> Exists in 7, not in 1
b -> Exists in 9, not in 3
c -> Take 4, remove b, d
d -> Exists in 8, not in 0
e -> Exists in 8, not in 9 OR Exists in 6 not in 5
f -> Take 1, remove c
g -> Take 0, remove 1 (c f), remove a, b, e


!#

(define (count-cipher-segments segments)
  (let ((cipher-lst (cadr segments)))
    (map string-length cipher-lst))) 

;; like assoc-ref but gets all values
(define (assoc-ref-all alist ref)
  (if (equal? alist '())
      '()
      (if (equal? (caar alist) ref)
	  (cons (cdar alist) (assoc-ref-all (cdr alist) ref))
	  (assoc-ref-all (cdr alist) ref))))
	  
	  
(define (first-pass digit-segments)
  (let ((segments (string-length digit-segments)))
    (case segments
      ((2) `(1 . ,digit-segments))
      ((4) `(4 . ,digit-segments))
      ((3) `(7 . ,digit-segments))
      ((7) `(8 . ,digit-segments))
      ((6) `(960 . ,digit-segments))
      (else `(#f . ,digit-segments)))))

(define (find-a segments-assoc)
  (format #t "~%segment-assoc: ~a" segments-assoc)
  (lset-difference eqv?
		   (string->list (assv-ref segments-assoc 7)) ;; MOST digits comes first in exp
		   (string->list (assv-ref segments-assoc 1)))) 

(define (find-bcdefg segments-assoc)
  (format #t "~%segment-assoc: ~a" segments-assoc)
  (let* ((960-segments (map string->list (assoc-ref-all segments-assoc 960)))
	 (8-segments (string->list (assv-ref segments-assoc 8)))
	 (1-segments (string->list (assv-ref segments-assoc 1)))
	 (4-segments (string->list (assv-ref segments-assoc 4)))
	 (extra-seg (map (cut lset-difference eqv? 8-segments <>) 960-segments)) ;; 
	 (f-list (map (cut lset-difference eqv? 1-segments <>) extra-seg))
	 (f (concatenate (filter (lambda (x) (eqv? (length x) 1)) f-list)))
	 (c (lset-difference eqv? 1-segments f))
	 (e-list (map (cut lset-difference eqv? <> 4-segments) extra-seg))
	 (e (concatenate (filter (lambda (x) (eqv? (length x) 1)) e-list)))
	 (d (concatenate (filter (lambda (x) (not (or (eqv? (car x) (car c)) (eqv? (car x) (car e))))) extra-seg)))
	 (b-candidates (lset-difference eqv? 4-segments 1-segments))
	 (b (concatenate (filter (lambda (x) (not (eqv? x (car d)))) b-candidates)))
	 (7-segments (string->list (assv-ref segments-assoc 7)))
	 (g-candidates (lset-difference eqv? 8-segments 4-segments 7-segments))
	 (g (concatenate (filter (lambda (x) (not (eqv? x (car e)))) g-candidates))))
    `((b . ,b) (c . ,(car c)) (d . ,(car d)) (e . ,(car e)) (f . ,(car f)) (g . ,g))))
	 

(define (determine-cryptograph one-line-segments)
  (let ((digit-lst (car one-line-segments)))
    (map first-pass digit-lst)))

(define (main args)
  (let* ((di (file->digit-inputs "test_input.txt"))
	 (ciphertext-segment-counts (concatenate (map count-cipher-segments di))))
    ;;(format #t "~%~%Input: ~a~%" ciphertext-segment-counts)
    ;; Part 1
    (format #t "~%~%Part 1: ~a~%" (count (lambda (n)
                                           (case n ((2 4 3 7) #t) (else #f)))
                                         ciphertext-segment-counts))
    ;; Part 2
    (let* ((number-assocs (map determine-cryptograph di))
	   (a-segs (map find-a number-assocs))
           (bcdefg-segs (map find-bcdefg number-assocs)) ;; Need to know the contents of 3 and 9 first!
           (all-segs (map (lambda (a others) (acons 'a (car a) others)) a-segs bcdefg-segs)))
      (format #t "~%a: ~a" a-segs)
      (format #t "~%bcdefg: ~a" bcdefg-segs)
      (format #t "~%all: ~a" all-segs))))

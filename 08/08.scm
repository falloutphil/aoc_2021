#!/usr/bin/env sh
exec guile -e '(@ (day08) main)' -s "$0" "$@"
!#

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
!#


(define-module (day08)
  #:export (main)
  #:use-module (ice-9 rdelim) ;; read-line
  #:use-module (srfi srfi-1) ;; concatenate, count
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

(define (count-cipher-segments segments)
  "Count the number of segments for a given digit."
  (let ((cipher-lst (cadr segments)))
    (map string-length cipher-lst))) 

(define (assoc-ref-all alist ref)
  "Like assoc-ref but gets all matching values for key."
  (if (equal? alist '())
      '()
      (if (equal? (caar alist) ref)
	  (cons (cdar alist) (assoc-ref-all (cdr alist) ref))
	  (assoc-ref-all (cdr alist) ref))))


(define (possible-digit-kv digit-segments)
  "Create an assoc list item of the form (possible-digits . coded-digit-segments)"
  (let ((segments (string-length digit-segments)))
    (case segments
      ((2) `(1 . ,digit-segments))
      ((4) `(4 . ,digit-segments))
      ((3) `(7 . ,digit-segments))
      ((7) `(8 . ,digit-segments))
      ((6) `(960 . ,digit-segments)) ;; could be a 9, 6, or 0
      (else `(#f . ,digit-segments))))) ;; otherwise don't care for this algo

(define (find-a segments-assoc)
  "It's easy to work out the value of segent a by finding the 
   difference between the segments in number 7 and number 1."
  ;;(format #t "~%a segment-assoc: ~a" segments-assoc)
  (lset-difference eqv?
		   (string->list (assv-ref segments-assoc 7)) ;; MOST digits comes first in exp
		   (string->list (assv-ref segments-assoc 1)))) 

;; yuk yuk! see comment at top which is mostly correct for how this works!
(define (find-bcdefg segments-assoc)
  "The rest of the segments can be worked out after a using the below
   fairly ugly algo!"
  ;;(format #t "~%bcdefg segment-assoc: ~a" segments-assoc)
  (let* ((960-segments (map string->list (assoc-ref-all segments-assoc 960)))
	 (8-segments (string->list (assv-ref segments-assoc 8)))
	 (1-segments (string->list (assv-ref segments-assoc 1)))
	 (4-segments (string->list (assv-ref segments-assoc 4)))
 	 ;; There are 3 transitions from 7 segments to 6
	 ;; 8->0, 8->6, and 8->9
	 ;; extra-seg gives the remaining segments after this
	 ;; transition in each case - we can then determine
	 ;; segments using numbers that only contain one of these
	 ;; possible 3 extra segments, which is enough to unravel the puzzle.
	 (extra-seg (map
		     (cut lset-difference eqv? 8-segments <>)
		     960-segments))
	 (f-list (map
		  (cut lset-difference eqv? 1-segments <>)
		  extra-seg)) ;; extra-seg = d,e,c
	 (f (concatenate (filter (lambda (x)
				   (eqv? (length x) 1))
				 f-list)))
	 (c (lset-difference eqv? 1-segments f))
	 (e-list (map (cut lset-difference eqv? <> 4-segments)
		      extra-seg))
	 (e (concatenate (filter (lambda (x)
				   (eqv? (length x) 1))
				 e-list)))
	 (d (concatenate (filter (lambda (x)
				   (not (or (eqv? (car x) (car c))
					    (eqv? (car x) (car e)))))
				 extra-seg)))
	 (b-candidates (lset-difference eqv? 4-segments 1-segments))
	 (b (concatenate (filter (lambda (x)
				   (not (eqv? x (car d))))
				 b-candidates)))
	 (7-segments (string->list (assv-ref segments-assoc 7)))
	 (g-candidates (lset-difference eqv? 8-segments 4-segments 7-segments))
	 (g (concatenate (filter (lambda (x)
				   (not (eqv? x (car e))))
				 g-candidates))))
    ;;(format #t "~%1-seg: ~a  f-list: ~a  f: ~a c: ~a" 1-segments f-list f c)
    `((,b . #\b) (,(car c) . #\c) (,(car d) . #\d) (,(car e) . #\e) (,(car f) . #\f) (,g . #\g))))


(define (determine-possible-digits one-line-segments)
  "Creates an assoc list of potential numbers and their coded representations."
  (let ((digit-lst (car one-line-segments))) ;; take the left of the pipe - coded numbers 0-9
    ;; create an assoc list of kv-pairs, assinging potential numbers to key sequences we need
    (map possible-digit-kv digit-lst)))

;; this is just the regular translation
;; from plaintext digit display to number
(define digit-assoc
  `((,(string->list "abcefg") . 0)
    (,(string->list "cf") . 1)
    (,(string->list "acdeg") . 2)
    (,(string->list "acdfg") . 3)
    (,(string->list "bcdf") . 4)
    (,(string->list "abdfg") . 5)
    (,(string->list "abdefg") . 6)
    (,(string->list "acf") . 7)
    (,(string->list "abcdefg") . 8)
    (,(string->list "abcdfg") . 9)))

(define (lsetv? x y)
  "Compare lsets in assoc lists. Used to compare unordered lists."
  (lset= eqv? x y))

(define (digit-as-list transform lst)
  "Find the decoded segment for each segment in a coded digit layout."
  ;;(format #t "~%transform: ~a lst: ~a" transform lst)
  (map (cut assv-ref transform <>) lst))

(define (decrypt input transform)
  "This applies the prepared cryptograph (transform) to the cypher (input)."
  (let* ((code (map string->list (cadr input))) ;; after the pipe
	 (clear (map (compose
		      number->string ;; string is easier to append
		      cdr ;; assoc returns (a . b), we only want b
		      ;; find integer from decoded digit pattern compare using lsetv? (list order is ignored)
		      (cut assoc <> digit-assoc lsetv?) 
		      (cut digit-as-list transform <>)) ;; decrypt using the provided assoc list "all-segs"
		     code))
	 (clear (string->number (apply string-append clear)))) ;; join the strings can convert back to number
    ;;(format #t "~%code: ~a" code)
    ;;(format #t "~%clear: ~a" clear)
    clear))



(define (main args)
  (let* ((di (file->digit-inputs "input.txt"))
	 (ciphertext-segment-counts (concatenate (map count-cipher-segments di))))
    ;; Part 1
    (format #t "~%~%Part 1: ~a~%" (count (lambda (n)
                                           (case n ((2 4 3 7) #t) (else #f)))
                                         ciphertext-segment-counts))
    ;; Part 2
    ;; First we create determine the mapping of possible digits to 0-9
    ;; Then we break each of these apart to first find a, then bcdefg.
    ;; These are then combined and sent to decrypt to transform to
    ;; the correct abcdefgh mappings (using all-segs), and ultimately
    ;; the equivalent number.
    (let* ((number-assocs (map determine-possible-digits di))
	   (a-segs (map find-a number-assocs))
           (bcdefg-segs (map find-bcdefg number-assocs)) 
           (all-segs (map (lambda (a others) (acons (car a) #\a others)) a-segs bcdefg-segs)))
      (format #t "~%~%Part 2: ~a~%" (reduce + 0 (map decrypt di all-segs)))
      )))

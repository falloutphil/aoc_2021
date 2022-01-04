#!/usr/bin/env sh
# -*- mode:scheme; geiser-scheme-implemenatation: guile -*-
exec guile -e '(@ (day14) main)' -s "$0" "$@"
!#

(define-module (day14)
  #:export (main)
  #:use-module (ice-9 rdelim)  ;; read-line
  #:use-module (ice-9 match)
  #:use-module (ice-9 hash-table)
  #:use-module (srfi srfi-1)   ;; last
  #:use-module (srfi srfi-11)  ;; let-values
  #:use-module (srfi srfi-26)  ;; cut
  #:use-module (srfi srfi-42)) ;; list-ec/eager comprehensions


(define (file->inputs filename)
  "Read first line the template, and
   remaining lines as assoc-list of:
   string of 2 chars -> char."
  (call-with-input-file filename
    (λ (p)
      (values (string->list (read-line p))
	      (cdr (list-ec (:port line p read-line) ;; cdr removes blank line
			    (match (string-tokenize line char-set:upper-case)
			      [() #f]
			      ;; find a better way to extract the char from the tokenizer!
			      [(pair insertion) (cons pair (car (string->list insertion)))]
			      [_ (error "bad rule!")])))))))

(define (insert template rules)
  "insert between each template item, the
   element provided by the rules assoc-list.
   Used in the naive solution to Part 1."
  (match template
    ;; match if we have 2 elements in the template
    [(x1 x2 . rest)
     ;; note we only add the LHS and the insertion element
     (cons (list x1 (assoc-ref rules (string x1 x2)))
	   ;; we then only drop x1 when recursing
	   ;; meaning we get x2 added on the next recursion
	   (insert (cdr template) rules))]
    ;; base case - if we don't have 2 elements to insert between we cap with the last element 
    [end (list end)]))


(define (increment-counter! ht k)
  "Increment count in hash table."
  (hash-set! ht k
	     (1+ (hashv-ref ht k 0))))

(define (omain args)
  (let-values ([(template rules) (file->inputs "test_input.txt")]
	       [atom-counter (make-hash-table)]
	       [pair-counter (make-hash-table)])
    (format #t "~%Template: ~a" template)
    (format #t "~%Rules: ~a" rules)
    (for-each (cut increment-counter! atom-counter <> ) template)))


(define (main args)
  (let-values ([(template rules) (file->inputs "input.txt")])
    ;;(format #t "~%Template: ~a" template)
    ;;(format #t "~%Rules: ~a" rules)
    (let* ([result    
	    (let loop ([n 10]
		       [t template])
	      (if (zero? n)
		  t ;; return latest template OR
		  ;; loop not in tail position.
		  ;; updated template is sent back into insert
		  ;; function 10 times.
		  (loop (1- n) (concatenate (insert t rules)))))]
	   [counter (make-hash-table)])
      ;; Create a dictionary of counts
      (for-each (cut increment-counter! counter <> ) result)
      ;; Find max and min dict values and subtract
      (format #t "~%Part 1: ~a~%" (apply - (hash-fold
					    (λ (_ v prior)
					      (match-let ([(max min) prior])
						(cond
						 [(> v max) (list v min)]
						 [(< v min) (list max v)]
						 [else prior])))
					    '(0 99999) counter)))))) ;; 99999 arbitrarily large


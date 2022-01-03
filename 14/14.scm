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
   element provided by the rules assoc-list."
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
	   [counter (make-hash-table)]
	   [max-kv 0] 
	   [min-kv 999999]) ;; arbitrarily large
      ;; Create a dictionary of counts
      (for-each
       (λ (e)
	 (hashv-set! counter e
		     (1+ (hashv-ref counter e 0))))
       result)
      ;; Find max and min
      (hash-for-each (λ (_ v)
		       (cond
			[(> v max-kv) (set! max-kv v)]
			[(< v min-kv) (set! min-kv v)]))
		     counter)
      (format #t "~%Part 1: ~a~%" (- max-kv min-kv)))))


#!/usr/bin/env sh
# -*- mode:scheme; geiser-scheme-implemenatation: guile -*-
exec guile -e '(@ (day14) main)' -s "$0" "$@"
!#

(define-module (day14)
  #:export (main)
  #:use-module (ice-9 rdelim)  ;; read-line
  #:use-module (ice-9 match) 
  #:use-module (srfi srfi-1)   ;; last
  #:use-module (srfi srfi-11)  ;; let-values
  #:use-module (srfi srfi-26)  ;; cut
  #:use-module (srfi srfi-42)) ;; list-ec/eager comprehensions


(define (file->inputs filename)
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
  (match template
    [(x1 x2 . rest)
     (cons (list x1 (assoc-ref rules (string x1 x2)))
	   (insert (cdr template) rules))]
    [end (list end)]))
	   
    

(define (main args)
  ;; call-with-values
  (let-values ([(template rules) (file->inputs "test_input.txt")])
    (format #t "~%Template: ~a" template)
    (format #t "~%Rules: ~a" rules)
    (format #t "~%Result: ~a"
	    (concatenate
	     (insert
	      (concatenate (insert template rules)) rules)))))

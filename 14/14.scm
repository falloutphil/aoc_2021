#!/usr/bin/env sh
# -*- mode:scheme; geiser-scheme-implemenatation: guile -*-
exec guile -e '(@ (day14) main)' -s "$0" "$@"
!#

(define-module (day14)
  #:export (main)
  #:use-module (ice-9 rdelim)  ;; read-line
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)   ;; first, second, last, list-copy
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
                              [(pair insertion) (cons (string->list pair)
						      (car (string->list insertion)))]
                              [_ (error "bad rule!")])))))))


(define-macro (increment-counter! al k)
  "Macro to avoid call-reference-by-value.
   Increment count in alist."
  `(set! ,al (assoc-set! ,al ,k
			 (1+ (or (assoc-ref ,al ,k) 0)))))

(define-macro (add-to-counter! al k v)
  "Macro to avoid call-reference-by-value.
   Add 'v' to the key's count, create if non-existent."
  `(set! ,al (assoc-set! ,al ,k
			 (+ (or (assoc-ref ,al ,k) 0) ,v))))

(define (assoc-copy lst)
  "1-deep list copy."
  (map list-copy lst))

(define (main args)
  (let-values ([(template rules) (file->inputs "test_input.txt")])
    (format #t "~%How many steps?: ")
    (let ([atom-counter '()]
          [pair-counter '()]
	  [max-steps (read)]) ;; ask the user for 10 or 40 
      (format #t "~%Template: ~a" template)
      (format #t "~%Rules: ~a" rules)
      (for-each (λ (e) (increment-counter! atom-counter e))
		template)
      (for-each (λ (e) (increment-counter! pair-counter e))
		(zip template (cdr template)))

      (format #t "~%start atom counter: ~a" atom-counter)
      (format #t "~%start pair counter: ~a" pair-counter)
      
      ;; loop over each step
      ;; NN - 2
      ;; CC - 2
      ;; BB - 2 
      ;; H - 1
      ;; ------------------
      ;; NN - 2 
      ;; BBBBBB - 6
      ;; CCCC - 4
      ;; H - 1
      ;; ------------------
      ;; NNNNN - 5
      ;; BBBBBBBBBBB - 11
      ;; CCCCC - 5
      ;; HHHH - 4

      (do ([steps 1 (1+ steps)])
	  ([> steps max-steps])
	(let ([step-pair-counter (assoc-copy pair-counter)]) ;; must keep list pristine as an input
	  (for-each (λ (rule)
		      (match-let* ([(pair . insertion) rule]
				   [original-pair-count (assoc-ref step-pair-counter pair)])
			(format #t "~%~%step-pair-counter: ~a" step-pair-counter)
			(format #t "~%pair: ~a" pair)
			(format #t "~%insertion: ~a" insertion)
			(format #t "~%original-pair-count: ~a" original-pair-count)
			(when original-pair-count
			  ;; for every count of the original pair we add to an insertion
			  ;; to the atom counter
			  (add-to-counter! atom-counter insertion original-pair-count)
			  ;; the old pairs no longer exist, so subtract them from total
			  (add-to-counter! pair-counter pair (- 0 original-pair-count))
			  (let ([new-lh-pair (list (first pair) insertion)]
				[new-rh-pair (list insertion (second pair))])
			    ;; for ever count of the original pair we add our new pairs
			    ;; to the pair counter
			    (add-to-counter! pair-counter new-lh-pair original-pair-count)
			    (add-to-counter! pair-counter new-rh-pair original-pair-count))))
		      (format #t "~%end pair counter: ~a" pair-counter)
                      (format #t "~%end atom counter: ~a~%" atom-counter))
		    rules)))
      
      )))

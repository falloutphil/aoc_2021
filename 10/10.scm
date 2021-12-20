#!/usr/bin/env sh
exec guile -e '(@ (day10) main)' -s "$0" "$@"
!#

(define-module (day10)
  #:export (main)
  #:use-module (oop goops) 
  #:use-module (ice-9 rdelim) ;; read-line
  #:use-module (srfi srfi-1) ;; concatenate
  #:use-module (srfi srfi-11) ;; let-values
  #:use-module (srfi srfi-26) ;; cut
  #:use-module (srfi srfi-42)) ;; list-ec

(define (file->list filename)
  "Read filename, and convert each line to a list."
  (call-with-input-file filename
    (lambda (p)
      (map string->list (list-ec (:port line p read-line) line)))))


;; filter for openners
;; convert to closers and reverse
;; should match filter for closers

(define (open-bracket? b-char)
  (case b-char
    ((#\( #\[ #\< #\{) #t)
    (else #f)))

(define (close-bracket? b-char)
  (case b-char
    ((#\) #\] #\> #\}) #t)
    (else #f)))

(define (translate-bracket b-char)
  ;;(format #t "~%b-char: ~a" b-char)
  (case b-char
    ((#\() #\))
    ((#\[) #\])
    ((#\<) #\>)
    ((#\{) #\})
    (else (error "impossible!"))))
    
(define-class <stack> ()
  (s #:init-value '()))

(define-method (push! (self <stack>) . args)
  (slot-set! self 's (append (reverse args)
			     (slot-ref self 's))))

(define-method (pop! (self <stack>))
  (let ((result (car (slot-ref self 's))))
    (slot-set! self 's (cdr (slot-ref self 's)))
    result))

(define-method (empty? (self <stack>))
  (null? (slot-ref self 's)))

;; pretty print stacks - https://www.wedesoft.de/software/2014/03/02/oop-with-goops/
(define-method (display (self <stack>) port)
  (format port "~a" (slot-ref self 's)))

(define (is-corrupt? bracket-list)
  (let ((stack (make <stack>)))
    (let loop ((bl bracket-list))
      ;;(format #t "~%stack: ~a, list: ~a" stack bl)
      (if (null? bl)
	  #f
	  (let ((bracket (car bl)))
	    (if (open-bracket? bracket)
		(begin
		  (push! stack bracket)
		  (loop (cdr bl)))
		(cond ;; closing bracket
		 ((empty? stack) #t) ;; if the stack is empty can't be closing!
		 ((eqv? bracket
			(translate-bracket (pop! stack)))
		  (loop (cdr bl)))
		 (else #t))))))))

	    
	      
(define (main args)
  (let* ((lofl-brackets (file->list "test_input.txt"))
	 (corrupt-lines (filter is-corrupt? lofl-brackets)))
    (format #t "~%initial list: ~a~%" (length lofl-brackets))
    (format #t "~%corrupt: ~a~%" (length corrupt-lines))))

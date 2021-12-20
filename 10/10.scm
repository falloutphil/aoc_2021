#!/usr/bin/env sh
exec guile -e '(@ (day10) main)' -s "$0" "$@"
!#

(define-module (day10)
  #:export (main)
  #:use-module (oop goops) 
  #:use-module (ice-9 rdelim) ;; read-line
  #:use-module (srfi srfi-1) ;; concatenate, filter-map, compose, append-reverse
  #:use-module (srfi srfi-26) ;; cut
  #:use-module (srfi srfi-42)) ;; list-ec

(define (file->list filename)
  "Read filename, and convert each line to a list."
  (call-with-input-file filename
    (lambda (p)
      (map string->list (list-ec (:port line p read-line) line)))))

(define (open-bracket? b-char)
  "#t for open brackets, #f for everything else."
  (case b-char
    ((#\( #\[ #\< #\{) #t)
    (else #f)))

(define (translate-bracket b-char)
  "Return corresponding closing bracket or error."
  ;;(format #t "~%b-char: ~a" b-char)
  (case b-char
    ((#\() #\))
    ((#\[) #\])
    ((#\<) #\>)
    ((#\{) #\})
    (else (error "impossible!"))))

;; LIFO Stack modeled as a list
(define-class <stack> ()
  (s #:init-value '() #:getter get-stack))

(define-method (push! (self <stack>) . args)
  "Push any args onto the given stack."
  (slot-set! self 's (append-reverse args ;; not strictly needed
				     (get-stack self))))

(define-method (pop! (self <stack>))
  "Pop returns the value it removes."
  (let ((result (car (slot-ref self 's))))
    (slot-set! self 's (cdr (get-stack self)))
    result))

(define-method (empty? (self <stack>))
  "Stack is empty?"
  (null? (get-stack self)))

;; pretty print stacks - https://www.wedesoft.de/software/2014/03/02/oop-with-goops/
(define-method (display (self <stack>) port)
  "Stack output for format and display."
  (format port "~a" (get-stack self)))

;; Scores for each corruption
(define bracket-points-1
  '((#\) . 3)
    (#\] . 57)
    (#\} . 1197)
    (#\> . 25137)))

;; Scores for each completion
(define bracket-points-2
  '((#\) . 1)
    (#\] . 2)
    (#\} . 3)
    (#\> . 4)))

(define (is-corrupt? bracket-list)
  "Does a given line (as a list) contain a corrupt bracket?"
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
		 ((empty? stack) 0) ;; if the stack is empty can't be closing!
		 ((eqv? bracket
			(translate-bracket (pop! stack)))
		  (loop (cdr bl)))
		 (else (assv-ref bracket-points-1
				 bracket))))))))) ;; return bad bracket rather than just #t


(define (complete-brackets bracket-list)
  "Complete any unclosed brackets"
  (let ((stack (make <stack>)))
    (let loop ((bl bracket-list))
      ;;(format #t "~%stack: ~a, list: ~a" stack bl)
      (if (null? bl)
	  (map translate-bracket (get-stack stack))
	  (let ((bracket (car bl)))
	    (if (open-bracket? bracket)
		(push! stack bracket)
		(pop! stack))
	    (loop (cdr bl)))))))

(define (main args)
  (let* ((lofl-brackets (file->list "input.txt"))
	 (corrupt-scores (filter-map is-corrupt? lofl-brackets))
	 (lofl-uncorrupt (filter (compose not is-corrupt?) lofl-brackets))
	 (completion-scores (map (cut fold (lambda (bracket sum)
					     (+ (* sum 5) (assv-ref bracket-points-2 bracket)))
				      0 <>)
				 (map complete-brackets lofl-uncorrupt))))
    ;;(format #t "~%initial list: ~a~%" lofl-brackets)
    (format #t "~%Part 1: ~a~%" (reduce + 0 corrupt-scores))
    (format #t "~%Part 2: ~a~%" ((compose car drop)
				 (sort completion-scores <)
				 (euclidean-quotient (length completion-scores) 2)))))

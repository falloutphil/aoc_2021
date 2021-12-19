#!/usr/bin/env sh
exec guile -e '(@ (day10) main)' -s "$0" "$@"
!#

(define-module (day10)
  #:export (main)
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

(define (open-bracket b-char)
  (case b-char
    ((#\( #\[ #\< #\{) #t)
    (else #f)))

(define (close-bracket b-char)
  (case b-char
    ((#\) #\] #\> #\}) #t)
    (else #f)))

(define (translate-bracket b-char)
  (format #t "~%b-char: ~a" b-char)
  (case b-char
    ((#\() #\))
    ((#\[) #\])
    ((#\<) #\>)
    ((#\{) #\})
    (else (error "impossible!"))))
    
    
(define (main args)
  (let* ((list-of-brackets (file->list "test_input.txt"))
	 (open-list (map (cut filter open-bracket <>) list-of-brackets))
	 (close-list (map (cut filter close-bracket <>) list-of-brackets)))
    (format #t "~%file: ~a~%" (file->list "test_input.txt"))
    (format #t "~%open: ~a~%" open-list)
    (format #t "~%close: ~a~%" close-list)
    (format #t "~%result1: ~a~%result2: ~a~%" (reverse (map (cut map translate-bracket <>) open-list)) close-list)))

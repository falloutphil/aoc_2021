#!/usr/bin/env -S guile -s
!#

;; requires guile-reader >= 0.63

(use-modules
 (system reader))

(define hpos 0)
(define vpos 0)

(define (forward n)
  (set! hpos (+ hpos n)))

(define (up n)
  (set! vpos (- vpos n)))

(define (down n)
  (set! vpos (+ vpos n)))

(define fn-read (token-reader-procedure
                 (standard-token-reader 'guile-symbol-lower-case)))

(define ws-read (token-reader-procedure
                 (standard-token-reader 'character))) ;; why not 'whitespace?

(define num-read (token-reader-procedure
                  (standard-token-reader 'character))) ;; why not 'guile-number?

(define (my-token-reader chr port reader top-level-reader)
  (let* ((fn (fn-read chr port reader))
	 (_ (ws-read chr port reader)) ;; read and ignore whitespace character
	 (num (- (char->integer (num-read chr port reader)) 48))) ;; crude char code -> number
    (list fn num)))

(define my-reader
  (make-reader (list (make-token-reader '(#\a . #\z) my-token-reader)
		     (make-token-reader '(#\space #\newline #\tab) #f))))

(load "input.txt" my-reader)
(format #t "~%Result: ~a~%" (* hpos vpos))





#!/usr/bin/env -S guile -s
!#

;; requires guile-reader >= 0.63

(use-modules
 (system reader))

(define hpos 0)
(define vpos 0)
(define aim 0)

(define (forward n)
  (set! hpos (+ hpos n))
  (set! vpos (+ vpos (* aim n))))

(define (up n)
  (set! aim (- aim n)))

(define (down n)
  (set! aim (+ aim n)))

(define (my-token-reader chr port reader top-level-reader)
  (let ((fn-read (token-reader-procedure
                  (standard-token-reader 'guile-symbol-lower-case)))
	(ws-read (token-reader-procedure
                  (standard-token-reader 'character)))
	(num-read (token-reader-procedure
                   (standard-token-reader 'character))))
    (let* ((fn (fn-read chr port reader))
	   (ws1 (ws-read chr port reader))
	   (num (- (char->integer (num-read chr port reader)) 48)))
      (list fn num))))

(define my-reader
  (make-reader (list (make-token-reader '(#\a . #\z) my-token-reader)
		     (make-token-reader '(#\space #\newline #\tab) #f))))

(load "input.txt" my-reader)
(format #t "~%Result: ~a~%" (* hpos vpos))





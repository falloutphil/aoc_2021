#!/usr/bin/env -S guile -s
!#

;; requires guile-reader >= 0.63

(use-modules
 (system reader)
 (ice-9 textual-ports))

(define hpos 0)
(define vpos 0)

(define (forward n)
  (set! hpos (+ hpos n)))

(define (up n)
  (set! vpos (- vpos n)))

(define (down n)
  (set! vpos (+ vpos n)))


;; combine 2 readers into a list, rather than combine 2 token readers?

;;(define (handle-fault chr port reader)
;;  (format #t "~%FAULT: *~a*" chr)
;;  " ")

(define ignore-chars
  (make-token-reader '(#\space #\newline #\tab) #f))

(define my-fn-reader (make-reader (list (standard-token-reader 'guile-symbol-lower-case))))
(define my-num-reader (make-reader (list ignore-chars
					 (standard-token-reader 'guile-number))))

(define (my-token-reader-proc chr port reader top-level-reader)
  (unget-char port chr)
  (let* ((fn (my-fn-reader port))
	 (num (my-num-reader port)))
    (format #t "~%FN: *~a*, Num: *~a*" fn num)
    (list fn num)))


(define my-reader
  (make-reader (list (make-token-reader '(#\a . #\z) my-token-reader-proc)
		     ignore-chars)))

(load "input.txt" my-reader)
(format #t "~%Result: ~a~%" (* hpos vpos))

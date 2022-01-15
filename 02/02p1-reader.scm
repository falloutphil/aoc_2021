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

(define my-fn-reader (make-reader (list (standard-token-reader 'guile-symbol-lower-case))))
(define my-num-reader (make-reader (list (make-token-reader #\space #f) ;; ignore space between func and number
					 (standard-token-reader 'guile-number))))

(define (my-token-reader-proc chr port reader top-level-reader)
  (unget-char port chr) ;; can returning the character to the port?
  ;; we use readers here not token-readers because readers
  ;; move our position in the port along, TRs don't?
  (let* ((fn (my-fn-reader port))
	 (num (my-num-reader port)))
    (list fn num)))

(define my-reader
  (make-reader (list (make-token-reader '(#\a . #\z) my-token-reader-proc)
		     (make-token-reader #\newline #f)))) ;; ignore newline immediately after parsing "func num"

(load "input.txt" my-reader)
(format #t "~%Result: ~a~%" (* hpos vpos))

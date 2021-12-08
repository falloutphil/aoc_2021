#!/usr/bin/env sh
exec guile -e '(@ (day04) main)' -s "$0" "$@"
!#

(define-module (day04)
  #:export (main)
  #:use-module (ice-9 rdelim) ;; read-line
  #:use-module (srfi srfi-1) ;; concatenate
  #:use-module (srfi srfi-11) ;; let-values
  #:use-module (srfi srfi-26) ;; cut
  #:use-module (srfi srfi-42)) ;; list-ec

(define (make-bingo-number n)
  (let ((called #f)
	(value n))
    (define (called?)
      called)
    (define (!called)
      (set! called #t))
    (define (get-value)
      value)

    (lambda args
      (apply
        (case (car args)
          ((called?) called?)
	  ((!called) !called)
	  ((get-value) get-value)
          (else (error "Invalid method!")))
        (cdr args)))))


(define (string->bingo-number s)
  (make-bingo-number (string->number s)))

(define (parse-input filename)
  "Read filename, first line to a list.
   Then place the rest into a 3D array."
  (call-with-input-file filename
    (lambda (p)
      (let* ((lines (list-ec (:port line p read-line) line))
	     (numbers (map string->number (string-split (car lines) #\,)))
	     (boards-str (delete '() (map (compose (cut delete "" <>) (cut string-split <> #\ ))
             				  (cdr lines))))
	     (flat-boards ((compose (cut list->array 1 <>) (cut map string->bingo-number <>) concatenate) boards-str)))
	;(format #t "~%numbers: ~a~%" numbers)
	;(format #t "~%boards-str: ~a~%" boards-str)
	(values
	 numbers
	 (make-shared-array flat-boards (lambda (i j k) (list (+ (* i 25) (* j 5) k))) 3 5 5))))))

(define (main args)
  (let-values (((numbers boards) (parse-input "test_input.txt")))
    (format #t "~%numbers: ~a~%" numbers)
    ;(format #t "~%first board: ~a~%" (array-cell-ref boards 0))
    (format #t "~%array values: ")
    (array-for-each (lambda (i) (format #t "(~a ~a) " (i 'get-value) (i 'called?))) boards)
    (format #t "~%test element: ~a~%" ((array-ref boards 0 0 0) 'get-value))))
    

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

;; use a continuation to break on bingo!
(define (slicer break boards)
  (let board-loop ((n 0))
    (let ((test-board (array-cell-ref boards n)))
      (let col-loop ((x 0))
	(let ((slice (make-shared-array test-board
			   (lambda (i) (list i x)) ;; take slice holding column constant as x
			   '(0 4)))
	      (bingo #t))
	  (array-for-each (lambda (i) (set! bingo (and bingo (i 'called?)))
				  (format #t "~%col slice: (~a ~a) " (i 'get-value) (i 'called?))) slice)
	  (format #t "~%bingo: ~a" bingo)
	  ;; if bingo break here!
	  (when bingo (break n))
	  )
	(when (< x 4) (col-loop (+ x 1))))
      (let row-loop ((x 0))
	(let ((slice (make-shared-array test-board
			   (lambda (i) (list x i)) ;; take slice holding row constant as x
			   '(0 4)))
	      (bingo #t))
	  (array-for-each (lambda (i) (set! bingo (and bingo (i 'called?)))
				  (format #t "~%row slice: (~a ~a) " (i 'get-value) (i 'called?))) slice)
	  (format #t "~%bingo: ~a" bingo)
	  ;; if bingo break here!
	  (when bingo (break n))
	  )
	(when (< x 4) (row-loop (+ x 1))))
    (when (< n 2) (board-loop (+ n 1))))))


(define (call-and-check break arr num)
  (array-for-each
   (lambda (i)
     (when (eqv? (i 'get-value) num)
       (i '!called))
     (format #t "~%num: ~a val: ~a called: ~a" num (i 'get-value) (i 'called?)))
   arr)
  (slicer break arr))
  
(define (main args)
  (let-values (((numbers boards) (parse-input "test_input.txt")))
    ;(format #t "~%numbers: ~a~%" numbers)
    ;(format #t "~%first board: ~a~%" (array-cell-ref boards 0))
    ;(format #t "~%array values: ")
    ;(array-for-each (lambda (i) (format #t "(~a ~a) " (i 'get-value) (i 'called?))) boards)
    ;(format #t "~%test element: ~a~%" ((array-ref boards 0 0 0) 'get-value))
    (let ((winning-board (call/cc (lambda (break)
			      (map (cut call-and-check break boards <>) numbers)))))
      (format #t "~%~%WINNING BOARD: ~a~%" winning-board))))
      ;(array-for-each (lambda (i) (format #t "(~a ~a) " (i 'get-value) (i 'called?))) slice))))
      
    

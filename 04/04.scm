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
  "Stateful number object that knows if it's been called."
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
	(values
	 numbers
	 (make-shared-array flat-boards (lambda (i j k) (list (+ (* i 25) (* j 5) k))) 100 5 5)))))) ;; HINT: grep '^$' input.txt | wc -l 


(define (bingo? break boards num)
  "Called after each number is drawn."
  "Loop over each bingo board."
  "Look at all column-wise and row-wise slices."
  "Exit via continuation if any col or row is all called." 
  (let board-loop ((n 0))
    (let ((test-board (array-cell-ref boards n)))
      ;; col-loop and row-loop should be refactored into one!
      (let col-loop ((x 0))
	(let ((slice (make-shared-array test-board
			   (lambda (i) (list i x)) ;; take slice holding column constant as x
			   '(0 4)))
	      (bingo #t))
	  (array-for-each (lambda (i) (set! bingo (and bingo (i 'called?))))
				  ;(format #t "~%col slice: (~a ~a) " (i 'get-value) (i 'called?)))
			  slice)
	  ;;(format #t "~%bingo: ~a" bingo)
	  (when bingo (break (list n num))))
	(when (< x 4) (col-loop (+ x 1))))
      (let row-loop ((x 0))
	(let ((slice (make-shared-array test-board
			   (lambda (i) (list x i)) ;; take slice holding row constant as x
			   '(0 4)))
	      (bingo #t))
	  (array-for-each (lambda (i) (set! bingo (and bingo (i 'called?))))
				  ;(format #t "~%row slice: (~a ~a) " (i 'get-value) (i 'called?)))
			  slice)
	  ;;(format #t "~%bingo: ~a" bingo)
	  (when bingo (break (list n num))))
	(when (< x 4) (row-loop (+ x 1))))
    (when (< n 99) (board-loop (+ n 1)))))) ;; don't forget to change me if the input file changes!


(define (call-and-check break arr num)
  "Loop over 3D array of boards."
  "For each number check against called number."
  "Set called on any object that matches."
  "Then check for bingo."
  (array-for-each
   (lambda (i)
     (when (eqv? (i 'get-value) num)
       (i '!called)))
     ;;(format #t "~%num: ~a val: ~a called: ~a" num (i 'get-value) (i 'called?)))
   arr)
  (bingo? break arr num))


(define (main args)
  (let-values (((numbers boards) (parse-input "input.txt")))
    (let* ((result (call/cc (lambda (break)
			      (map (cut call-and-check break boards <>) numbers))))
	   (winning-idx (car result))
	   (winning-num (cadr result)))
      (format #t "~%~%winning index: ~a, winning number: ~a~%" winning-idx winning-num)
      (let ((winning-board (array-cell-ref boards winning-idx))
	    (unmarked-sum 0))
	(array-for-each (lambda (i)
			  (format #t "(~a ~a) " (i 'get-value) (i 'called?))
			  (when (not (i 'called?)) (set! unmarked-sum (+ unmarked-sum (i 'get-value)))))
			winning-board)
	(format #t "~%~%unmarked sum: ~a~%" unmarked-sum)
	(format #t "~%~%final score: ~a~%" (* unmarked-sum winning-num))))))

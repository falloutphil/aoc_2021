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


(define (check-line bingo-action board-idx test-board direction called-number)
  "Check bingo lines for a specific board in a specific direction for one called number."
  (let line-loop ((x 0))

    (define (make-slicer direction)
      "Slice a column or row at position i."
      (case direction
        ((col) (lambda (i) (list i x)))
        ((row) (lambda (i) (list x i)))
        (else (error "Invalid direction!"))))

    (let ((slice (make-shared-array test-board
                                    (make-slicer direction) ;; take slice holding row/col constant
                                    '(0 4)))
          (bingo #t))
      (array-for-each (lambda (i)
                        ;(format #t "~%~a slice: (~a ~a) " direction (i 'get-value) (i 'called?))
                        (set! bingo (and bingo (i 'called?))))
                      slice)
      ;(format #t "~%bingo: ~a" bingo)
      (when bingo
        (bingo-action `(,board-idx . ,called-number))))
    (when (< x 4) (line-loop (+ x 1)))))

(define (bingo? bingo-action boards num)
  "Called after each number is drawn."
  "Loop over each bingo board."
  "Look at all column-wise and row-wise slices."
  "Exit via continuation if any col or row is all called." 
  (let board-loop ((n 0))
    (let ((test-board (array-cell-ref boards n)))
      (check-line bingo-action n test-board 'col num)
      (check-line bingo-action n test-board 'row num)
      (when (< n 99) (board-loop (+ n 1)))))) ;; don't forget to change me if the input file changes!


(define (call-and-check bingo-action arr num)
  "Loop over 3D array of boards."
  "For each number check against called number."
  "Set called on any object that matches."
  "Then check for bingo."
  (array-for-each
   (lambda (i)
     ;(format #t "~%num: ~a val: ~a called: ~a" num (i 'get-value) (i 'called?))
     (when (eqv? (i 'get-value) num)
       (i '!called)))
   arr)
  (bingo? bingo-action arr num))


(define (calc-unmarked-sum boards idx)
  "Calculate the unmarked sum of the board at the index."
  (let ((board (array-cell-ref boards idx))
	(unmarked-sum 0))
    (array-for-each (lambda (i)
		      (format #t "(~a ~a) " (i 'get-value) (i 'called?))
                      (when (not (i 'called?))
			(set! unmarked-sum (+ unmarked-sum (i 'get-value)))))
                    board)
    unmarked-sum))

;; There's nothing like lazily jamming a part 2 solution into a part 1 framework!
(define (make-bingo-logger boards)
  "Keeps a log of the first bingo call for each board in an assoc list.
   Must calculate the unmarked-sum immediately whilst board state is correct.
   The last unmarked-sum must be the last 'first bingo' for a board.
   Returns the assoc list and unmarked sum when passed dummy #f."
  (let ((bingo-assoc '())
	(unmarked-sum 0))
    (lambda (result)
      ;; check bingo hasn't been called for this board
      (when (and result (not (assoc (car result) bingo-assoc)))
        (set! bingo-assoc (cons result bingo-assoc))
	(set! unmarked-sum (calc-unmarked-sum boards (car result))))
      `(,bingo-assoc . ,unmarked-sum))))

(define (main args)
  (let-values (((numbers boards) (parse-input "input.txt")))
    ;; Part 1
    (let* ((result (call/cc (lambda (break)
                              (for-each (cut call-and-check break boards <>) numbers)))) ;; main entry point
           (winning-idx (car result))
           (winning-num (cdr result)))
      (format #t "~%~%winning index: ~a, winning number: ~a~%" winning-idx winning-num)
      (let ((unmarked-sum (calc-unmarked-sum boards winning-idx)))
        (format #t "~%~%unmarked sum: ~a~%" unmarked-sum)
        (format #t "~%~%part 1 final score: ~a~%~%" (* unmarked-sum winning-num)))))
  
  (let-values (((numbers boards) (parse-input "input.txt")))
    ;; Part 2
    (let ((logger (make-bingo-logger boards)))
      (for-each (cut call-and-check logger boards <>) numbers) ;; main entry point
      (let* ((logger+unmarked (logger #f))
	     (result (caar logger+unmarked))
	     (unmarked-sum (cdr logger+unmarked))
             (last-idx (car result))
             (last-num (cdr result)))
        (format #t "~%~%last index: ~a, last number: ~a~%" last-idx last-num)
        (format #t "~%~%unmarked sum: ~a~%" unmarked-sum)
        (format #t "~%~%part 2 final score: ~a~%" (* unmarked-sum last-num))))))

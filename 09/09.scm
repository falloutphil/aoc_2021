#!/usr/bin/env sh
exec guile -e '(@ (day09) main)' -s "$0" "$@"
!#

(define-module (day09)
  #:export (main)
  #:use-module (ice-9 rdelim) ;; read-line
  #:use-module (srfi srfi-1) ;; concatenate
  #:use-module (srfi srfi-9) ;; records
  #:use-module (srfi srfi-11) ;; let-values
  #:use-module (srfi srfi-26) ;; cut
  #:use-module (srfi srfi-42)) ;; list-ec

(define (parse-input filename)
  "Read file of numbers into 2D array."
  (call-with-input-file filename
    (lambda (p)
      (list->array 2 (map (lambda (str)
			    (map (compose string->number string) (string->list str)))
		          (list-ec (:port line p read-line) line))))))

;; Holds masking grid and it's central location.
(define-record-type <centred-mask>
  (make-centred-mask grid i j)
  centred-mask?
  (grid centred-mask-grid)
  (i centred-mask-i)
  (j centred-mask-j))

;; j-1 < 0, i-1 < 0 ---- top left make array (i i+1) (j j+1)
;; j-1 < 0 ---- left edge of row  make array (i-1 i+1) (j j+1)
;; i-1 < 0 ---- top row  make array (i i+1) (j-1 j+1)
;; j-1->j+1 and i-1->i+1 exists, make array (i-1 i+1) (j-1 j+1)
;; j+1 > len, i+1 > len ---- bottom right make array (i-1 i) (j-1 j)
;; j+1 > len ---- right edge of row make array (i-1 i+1) (j-1 j)
;; i+1 > len ---- bottom row make array (i-1 i) (j-1 j+1)
;; use check bounds so we can use same routine for all situations
(define (list+inc i)
  (list i (1+ i)))

(define (list+dec i)
  (list (1- i) i))

(define (list+-inc i)
  (list (1- i) (1+ i)))

(define (make-sub-grid world)
  (lambda (i-bounds j-bounds)
    ;; list is just a pass-through so we don't change
    ;; the coordinate system.  The idea is to use
    ;; array-for-each or similar to traverse the array
    ;; without coordinates.
    ;; The trick will be filtering out all the diagonals!
    (format #t "~%i bounds: ~a j bounds: ~a" i-bounds j-bounds)
    (make-shared-array world list i-bounds j-bounds)))


;; need a function to pass to array-for-each
;; or perhaps array->list and work out if the central
;; value a low value in it's locality.

;; should be a structure that knows it's centre.
(define top-left
  (make-centred-mask #2((#f #t) (#t #f)) 0 0))

(define top-right
  (make-centred-mask #2((#t #f) (#f #t)) 0 1))

(define bottom-left
  (make-centred-mask #2((#t #f) (#f #t)) 1 0))

(define bottom-right
  (make-centred-mask #2((#f #t) (#t #f)) 0 1))

(define top-row
  (make-centred-mask #2((#t #f #t) (#f #t #f)) 0 1))

(define bottom-row
  (make-centred-mask #2((#f #t #f) (#t #f #t)) 1 1))

(define left-col
  (make-centred-mask #2((#t #f) (#f #t) (#t #f)) 1 0))

(define right-col
  (make-centred-mask #2((#f #t) (#t #f) (#f #t)) 1 1))

(define other
  (make-centred-mask #2((#f #t #f) (#t #f #t) (#f #t #f)) 1 1))


(define (low-point? grid-pair)
  (let* ((result #t)
         (sub (cdr grid-pair))
         (c-mask (car grid-pair))
         (c (array-ref sub (centred-mask-i c-mask) (centred-mask-j c-mask)))) ;; value @ centre
    (format #t "~%grid: ~a" (centred-mask-grid c-mask))
    (format #t "~%i: ~a" (centred-mask-i c-mask))
    (format #t "~%j: ~a"  (centred-mask-j c-mask))
    (format #t "~%centre: ~a" c)
    (array-for-each (lambda (sub-v mask-v)
                      (format #t "~%sub-v: ~a mask-v: ~a" sub-v mask-v)
                      (when mask-v (set! result (and result (< c sub-v))))) ;; the key line!
                    sub (centred-mask-grid c-mask))
    result))


(define (adjacent-grid world i j)
  "Return grid centered on (i j)."
  (let* ((dims (array-dimensions world))
	 (cols (1- (cadr dims)))
	 (rows (1- (car dims)))
	 (sub-grid (make-sub-grid world)))
    (format #t "~%cols: ~a rows: ~a" cols rows)
    (cond ;; order is important!
     ;; top left
     ((and (< (1- i) 0) (< (1- j) 0)) `(,top-left . ,(sub-grid (list+inc i) (list+inc j))))
     ;; top right
     ((and (< (1- i) 0) (> (1+ j) cols)) `(,top-right . ,(sub-grid (list+inc i) (list+dec j))))
     ;; bottom left
     ((and (> (1+ i) rows) (< (1- j) 0)) `(,bottom-left . ,(sub-grid (list+dec i) (list+inc j))))
     ;; bottom right
     ((and (> (1+ i) rows) (> (1+ j) cols)) `(,bottom-right . ,(sub-grid (list+dec i) (list+dec j))))
     ;; top row
     ((< (1- i) 0) `(,top-row . ,(sub-grid (list+inc i) (list+-inc j))))
     ;; bottow row
     ((> (1+ i) rows) `(,bottom-row . ,(sub-grid (list+dec i) (list+-inc j))))
     ;; left col
     ((< (1- j) 0) `(,left-col . ,(sub-grid (list+-inc i) (list+inc j))))
     ;; right col
     ((> (1+ j) cols) `(,right-col . ,(sub-grid (list+-inc i) (list+dec j))))
     ;; elsewhere, not on an edge
     (else `(,other . ,(sub-grid (list+-inc i) (list+-inc j)))))))

(define (main args)
  (let* ((world (parse-input "test_input.txt"))
	 (test-grid (adjacent-grid world 2 2)))
    (format #t "~%world: ~a~%" world)
    (format #t "~%test-grid: ~a~%" test-grid)
    (format #t "~%TL+1 low point?: ~a~%" (low-point? test-grid))))


#!

TOP LEFT

c  #t

#t #f

TOP RIGHT

#t  c

#f  #t

BOTTOM LEFT

#t  #f

#c  #t

BOTTOM RIGHT

#f  #f

#t  #c

LEFT COL

#t #f

c  #t

#t #f

RIGHT COL

#f #t

#t c

#f #t

TOP ROW

#t c  #t
#f #t #f

BOTTOM ROW

#f #t #f
#t c  #t

OTHERWISE

#f #t #f
#t c  #t
#f #t #f

scheme@(guile-user)> (make-shared-array a (lambda (i j) (list i j)) '(0 2) '(0 2))
$6 = #2((2 1 9) (3 9 8) (9 8 5))
scheme@(guile-user)> (make-shared-array a (lambda (i j) (list i j)) '(0 2) '(1 3))
$7 = #2@0@1((1 9 9) (9 8 7) (8 5 6))
scheme@(guile-user)> (make-shared-array a (lambda (i j) (list i j)) '(0 2) '(2 4))
!#

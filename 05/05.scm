#!/usr/bin/env sh
exec guile -e '(@ (day05) main)' -s "$0" "$@"
!#

(define-module (day05)
  #:export (main)
  #:use-module (ice-9 rdelim) ;; read-line
  #:use-module (srfi srfi-1) ;; iota, last
  #:use-module (srfi srfi-26) ;; cut
  #:use-module (srfi srfi-42)) ;; list-ec

(define (split-coord coord-str)
  (map string->number (string-split coord-str #\,)))

(define (file->coords filename)
  "Read filename, and convert each line to a list.
   Then split on space and process first and last elements as coords."
  (call-with-input-file filename
    (lambda (p)
      (map (compose (lambda (str-lst)
                      (list (split-coord (car str-lst))
                            (split-coord (last str-lst))))
                    (cut string-split <> #\ ))
           (list-ec (:port line p read-line) line)))))

(define (expand-pair points)
  "Expand every vertical or horizontal point between a pair of points."
  (let* ((x1 (caar points))
         (y1 (cadar points))
         (x2 (caadr points))
         (y2 (cadadr points))
         (high-x (max x1 x2))
         (low-x (min x1 x2))
         (high-y (max y1 y2))
         (low-y (min y1 y2))
         (xs (iota (- high-x low-x -1) low-x))
         (ys (iota (- high-y low-y -1) low-y))
         (len-x (length xs))
         (len-y (length ys)))
    (apply zip (cond ((> len-x len-y) (list xs (iota len-x (car ys) 0)))
                     ((< len-x len-y) (list (iota len-y (car xs) 0) ys))
		     (else (let ((xs (if (eqv? (car xs) x1) xs (reverse xs)))
				 (ys (if (eqv? (car ys) y1) ys (reverse ys))))
			     (list xs ys)))))))

(define (consecutive? points)
  "Does x=x or y=y across the two coords?"
  (or (eqv? (caar points) (caadr points))
      (eqv? (cadar points) (cadadr points))))

(define (diagonal? points)
  ((compose not consecutive?) points))

(define (>1 _ v) (> v 1))

(define (main args)
  (let* ((coords (file->coords "input.txt"))
         (consec-coords (filter consecutive? coords))
	 (diagonal-coords (filter diagonal? coords))
         (p1-points (concatenate (map expand-pair consec-coords)))
	 (p2-points (concatenate (map expand-pair diagonal-coords))))
    (format #t "~%~%coords: ~a~%" coords)
    (format #t "~%~%consecutive coords: ~a~%" consec-coords)
    (format #t "~%~%diagonal coords: ~a~%" diagonal-coords)
    (format #t "~%~%p1 points: ~a~%" p1-points)
    (format #t "~%~%p2 points: ~a~%" p2-points)
    (let ((point-count (make-hash-table 500)))
       ;; part 1
      (for-each
       (lambda (key) (hash-set! point-count key
                                (+ (hash-ref point-count key 0) 1)))
       p1-points)
      (format #t "~%~%part 1: ~a~%"
	      (hash-count >1 point-count))

      ;; part 2
      (for-each
       (lambda (key) (hash-set! point-count key
                                (+ (hash-ref point-count key 0) 1)))
       p2-points)
      (format #t "~%~%part 2: ~a~%"
	      (hash-count >1 point-count)))))

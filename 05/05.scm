#!/usr/bin/env sh
exec guile -e '(@ (day05) main)' -s "$0" "$@"
!#

(define-module (day05)
  #:export (main)
  #:use-module (ice-9 rdelim) ;; read-line
  #:use-module (srfi srfi-1) ;; iota, last
  #:use-module (srfi srfi-11) ;; let-values
  #:use-module (srfi srfi-26) ;; cut
  #:use-module (srfi srfi-42)) ;; list-ec


;; for each horizontal/vertical line find points

;; these points can just be stored in a hash map

;; each value of hash map can be incremented

;; then we filter the hash map by values >1

;; the fact it is a coordinate system is irrelevant,
;; this is just a count how many things exist at each
;; symbol, and a need to generate symbols between points.


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
  "Expand every vertical or horizontal point between sets of points."
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
    (apply zip (if (> len-x len-y)
                   (list xs (iota len-x (car ys) 0))
                   (list (iota len-y (car xs) 0) ys)))))

(define (consecutive? points)
  "Does x=x or y=y across the two coords?"
  (or (eqv? (caar points) (caadr points))
      (eqv? (cadar points) (cadadr points))))

(define (main args)
  (let* ((coords (file->coords "test_input.txt"))
         (consec-coords (filter consecutive? coords))
         (all-points (concatenate (map expand-pair consec-coords))))
    (format #t "~%~%coords: ~a~%" coords)
    (format #t "~%~%consecutive coords: ~a~%" consec-coords)
    (format #t "~%~%all coords: ~a~%" all-points)))

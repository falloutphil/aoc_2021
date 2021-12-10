#!/usr/bin/env sh
exec guile -e '(@ (day05) main)' -s "$0" "$@"
!#

(define-module (day05)
  #:export (main)
  #:use-module (ice-9 rdelim) ;; read-line
  #:use-module (srfi srfi-1) ;; concatenate
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
   Then replace all 0s with #f."
  (call-with-input-file filename
    (lambda (p)
      (map (compose (lambda (str-lst)
		      (list (split-coord (car str-lst))
			    (split-coord (last str-lst))))
		    (cut string-split <> #\ ))
           (list-ec (:port line p read-line) line)))))

(define (main args)
  (let ((coords (file->coords "test_input.txt")))
    (format #t "~%~%coords: ~a~%" coords)))
  

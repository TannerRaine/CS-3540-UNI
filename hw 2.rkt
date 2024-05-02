;;
;; FILE:     homework02.rkt
;; AUTHOR:   Tanner Raine :)
;; DATE:     1/23/2024
;; COMMENT:  Provides templates for your solutions, plus a few tests.
;;
;; MODIFIED: 1/24/2024 by Tanner Raine
;; CHANGE:   Problem 5 code, made signage inclusive, forgot about that! Sorry!
;;

#lang racket
(require rackunit)      ; enables you to use rackunit tests

; -------------------
; -----   [1]   -----
; -------------------

(define total-grade
  (lambda (homework quiz final)
    (+ (* .3 homework ) (* .4 quiz ) (* .3 final) )) )                ; replace the 0 with your code

(check-= (total-grade  90  80  90)  86 0.1)
(check-= (total-grade  60  50  40)  50 0.1)
(check-= (total-grade 100 100 100) 100 0.1)

; -------------------
; -----   [2]   -----
; -------------------

(define price-per-ounce
  (lambda (units-per-pack ounces-per-unit pack-price)
    (/ pack-price (* units-per-pack ounces-per-unit)) ))                ; replace the 0 with your code

(check-= (price-per-ounce 6 24   1.44) 0.010 0.01)
(check-= (price-per-ounce 6 16.9 1.44) 0.014 0.01)

; -------------------
; -----   [3]   -----
; -------------------

(define ring-area
  (lambda (inner outer)
    (- (* pi (* outer outer)) (* pi (* inner inner)) )))                ; replace the 0 with your code

(check-= (ring-area 1 2)  9.42     0.01)
(check-= (ring-area 2 2)  0        0.01)
(check-= (ring-area 5 48) 7159.689 0.01)

; -------------------
; -----   [4]   -----
; -------------------

(define candy-temperature
  (lambda (temp elevation)
    (- temp (/ elevation 500) )))                ; replace the 0 with your code

(check-= (candy-temperature 244 5280)   233.44    0.00001)
(check-= (candy-temperature 302 977.69) 300.04462 0.00001)
(check-= (candy-temperature 302 -1401)  304.802   0.00001)

; -------------------
; -----   [5]   -----
; -------------------

(define in-range?
  (lambda (actual desired epsilon)
    (if (>= desired actual)  ;if desired is larger than actual
         (<= (- desired epsilon) actual) ;then, desired w/ tolerance STILL larger, False
         (>= (+ desired epsilon) actual) )))  ;else, desired w/ tolerance larger = False

(check-equal? (in-range? 4.95 5.0 0.1)  #t)
(check-equal? (in-range? 4.95 5.0 0.01) #f)     ;; not anymore!
(check-equal? (in-range? 5.0 4.95 0.1)  #t)     ;; works both ways
(check-equal? (in-range? 5.0 5.95 0.1)  #f)
(check-equal? (in-range? 5.5 5.95 0.5)  #t)

; -----   end   -----
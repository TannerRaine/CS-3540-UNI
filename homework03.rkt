;;
;; FILE:     homework03.rkt
;; AUTHOR:   Tanner Raine
;; DATE:     1/30/2024
;; COMMENT:  Includes rackunit tests for the examples in the assignment.
;;
;; MODIFIED: 
;; CHANGE:   
;;

#lang racket
(require rackunit)

; -----   [1]   -----

(define price-per-ounce-for
  (lambda (units-per-pack ounces-per-unit)
    (lambda (price)
      (/ price (* units-per-pack ounces-per-unit )) )))

(define big-bottles-at (price-per-ounce-for 6 24))
(check-= ((price-per-ounce-for 6 16.9) 1.44) 0.01420118 0.0001)
(check-= (big-bottles-at 1.44) 0.01 0.0001)
(check-= (big-bottles-at 14.4) 0.1 0.001)
(check-= (big-bottles-at 144.0) 1.0 0.01)

; -----   [2]   -----

(define circle-area
  (lambda (r)
    (* pi (expt r 2))))

(define ring-area-around
  (lambda (inner)
    (lambda (ro)
      (- (circle-area ro) (circle-area inner)) )))

(define area-for-2inch-bolt (ring-area-around 2))
(check-= ((ring-area-around 1) 2)  9.42477796076938 0.0001)
(check-= (area-for-2inch-bolt 5)  65.97344572538566 0.0001)
(check-= (area-for-2inch-bolt 10) 301.59289474462014 0.0001)
(check-= ((ring-area-around 4) 8) 150.79644737231007 0.0001)

; -----   [3]   -----

(define average-weight
  (lambda (height-weight-list)
    (apply average (map cdr height-weight-list)) ))

(define average               ; a helper function
  (lambda numbers
    (/ (apply + numbers)
       (length numbers))))

(check-= (average-weight '((79 . 225) )) 225.0 0.0001)
(check-= (average-weight '((70 . 150) (62 . 100) )) 125.0 0.0001)

; -----   [4]   -----

(define total-margin
  (lambda (list-of-pairs)
    (apply + (map abs
           (map -
         (map first list-of-pairs) ( map second list-of-pairs))))))
    ;(+ (apply + (map first list-of-pairs)) (apply + (map second list-of-pairs)) )))


(define uni-women
  '((102 51) (78 67) (53 94) (64 75) (54 71) (64 68)
    (65 84) (62 115) (59 78) (70 87) (52 54) (82 52)
    (85 79) (76 52) (95 75) (67 72) (77 88) (105 59)))

(check-equal? (total-margin '((102 51) (78 67) (53 94)))
              103)
(check-equal? (total-margin uni-women)
              387)

; -----   [5]   -----

(define min-open-seats
  (lambda (sections)
     (map string->number (map rest (rest sections)))))

(define example '(("Dept" "Number" "Section" "Class Nbr" "Capacity" "Enrolled")
                  ("CS" "1000" "1" "11546" "30" "30")
                  ("CS" "1025" "1" "11547" "30" "30")
                  ("CS" "1120" "1" "11557" "30" "15")
                  ("CS" "1130" "1" "11548" "30" "18")))
(check-equal? (min-open-seats example) 0)

; -----   end   -----

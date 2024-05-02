;;
;; FILE:     homework05-tests.rkt
;; AUTHOR:   Tanner Raine
;; DATE:     2/21/2024
;; COMMENT:  This file loads "homework05.rkt" and runs tests on its
;;           publicly-defined functions.
;;
;; MODIFIED: 2/22/2024
;; CHANGE:   Added test code for Problem 4
;;

#lang racket
(require rackunit)
(require "homework05.rkt")

;; --------------------------------------------------------------------------
;; Problem 1                                           (structural recursion)
;; --------------------------------------------------------------------------

(check-true (prefix-of? '(cs1510 cs1800) '(cs1510 cs1800 cs1520 cs1410)))
(check-false (prefix-of? '(cs1800 cs1510) '(cs1510 cs1800 cs1520 cs1410)))
(check-true (prefix-of? '() '(cs1510 cs1800 cs1520 cs1410))) ;empty lst1
(check-false (prefix-of? '(cs1510 cs1800) '() )) ;empty lst2

;; --------------------------------------------------------------------------
;; Problem 2                                               (mutual recursion)
;; --------------------------------------------------------------------------

(check-true (includes-course? 'cs1510
                              '((cs1120 cs1130 cs1140 cs1150 cs1160) cs1510 ((cs1410 cs2420) (cs1800 cs2530)))))

(check-false (includes-course? 'cs1510
                               '(()) )) ;empty nested list

(check-false (includes-course? 'cs3540
                    '(cs1120 cs1130 cs1140 cs1150 cs1160 cs1510)))

;; --------------------------------------------------------------------------
;; Problem 3                                               (mutual recursion)
;; --------------------------------------------------------------------------

(check-equal? (nlist+ '((3 4 ) (1) 3 4 5)) 20)
(check-equal? (nlist+ '()) 0)

;; --------------------------------------------------------------------------
;; Problem 4                                               (mutual recursion)
;; --------------------------------------------------------------------------

(check-equal? (max-length '("Write" "a" "mutually" "recursive" "function"
                ("max-length" "str-list")
                "that" "takes" "one" "argument"
                ("a" "string-list"))) 11)


;; --------------------------------------------------------------------------
;; Problem 5                                               (mutual recursion)
;; --------------------------------------------------------------------------

(check-equal? (prefix->infix '(+ (+ 2 2) (* 2 2))) '((2 + 2) + (2 * 2)))
(check-equal? (prefix->infix '(+ 2 2)) '(2 + 2))
(check-equal? (prefix->infix '()) '())

;; --------------------------------------------------------------------------


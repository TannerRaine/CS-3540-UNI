;;
;; FILE:     homework04.rkt
;; AUTHOR:   Tanner Raine
;; DATE:     2/14/2024
;; COMMENT:  This module defines the five functions specified in
;;           Homework 4 as an importable module.
;;
;; MODIFIED: 12/15/2024 by Tanner Raine
;; CHANGE:   Added test code for Problem 5 and comments for Problem 5
;;

#lang racket
(require rackunit)      ; enables you to use rackunit tests
(provide every?         ; exports your functions to client code
         reject         ;      You must define a function with
         interleave     ;      each name, even if the function
         cons-at-end    ;      only returns a default value!
         positions-of)

;; --------------------------------------------------------------------------
;; Problem 1                                           (structural recursion)
;; --------------------------------------------------------------------------

(define every?
  (lambda (lob)
    (if (null? lob) ;empty list
        #f
        (if (eq? (first lob) #f) ;found false, drop out
            #f
            (if (eq? (list-length lob) 1) ;only one item left, not false
                #t
                (every? (rest lob)) ;recursion if more than one item left in list
            )))))

(define list-length ;helper function
  (lambda (lon) 
    (if (null? lon)
        0
        (+ 1 (list-length (rest lon))))))

(check-false (every? '(#t #f)))
(check-false (every? '()))
(check-true (every? '(#t #t #t)))
(check-false (every? '(#t #t #t #f)))
(check-true (every? '(#t)))
(check-false (every? '(#f)))


;; --------------------------------------------------------------------------
;; Problem 2                                           (structural recursion)
;; --------------------------------------------------------------------------

(define reject
  (lambda (pred? lst)
    (if (null? lst) ;list empty
        '()
        (if (pred? (first lst)) ;if matches pred (neg, pos, etc)
            (reject pred? (rest lst))
            (cons (first lst) ;appends first item to new list
                  (reject pred? (rest lst)))))))
    


(check-equal? (reject negative? '(1 2 3 4)) '(1 2 3 4))
(check-equal? (reject negative? '(-1 -2 -3 -4)) '())
(check-equal? (reject negative? '(1 -2 3 -4)) '(1 3))
(check-equal? (reject negative? '()) '())
;; --------------------------------------------------------------------------
;; Problem 3                                           (structural recursion)
;; --------------------------------------------------------------------------

(define interleave
  (lambda (lst1 lst2)
    (if (null? lst1) ;empty list1
        '()
        (if (null? lst2) ;empty list2
            '()
            (cons (cons (first lst1) (first lst2)) ;append recursion & pair append
                  (interleave (rest lst1) (rest lst2)) ))

    )))

(check-equal? (interleave '(a b c d) '(1 2 3 4)) '((a . 1) (b . 2) (c . 3) (d . 4)))
(check-equal? (interleave '(a b c d e) '(1 2 3 )) '((a . 1) (b . 2) (c . 3)))
(check-equal? (interleave '() '(1 2 3 4)) '())
(check-equal? (interleave '(a b c d) '()) '())
(check-equal? (interleave '() '()) '())


;; --------------------------------------------------------------------------
;; Problem 4                                           (structural recursion)
;; --------------------------------------------------------------------------

(define cons-at-end
  (lambda (v lst)
    (if (null? lst) ;if empty, just throw v on there as a list
        (list v) 
        (if (=(list-length (rest lst)) 0) ;if none left, add v
            (list (first lst) v) ;adds v as a list and not a pair
            (cons (first lst)
                  (cons-at-end v (rest lst))) ;recursion
            ))))

(check-equal? (cons-at-end 'e '(a b c d)) '(a b c d e))
(check-equal? (cons-at-end 'e '() ) '(e))

;; --------------------------------------------------------------------------
;; Problem 5                   (structural recursion and interface procedure)
;; --------------------------------------------------------------------------

(define helper
  (lambda (s los constant holder)
    (if (null? los)
        holder
    (if (eq? (first los) s)
        (cons constant (helper s (rest los) (+ 1 constant) holder)) ;add int and add number to list, recursion
        (helper s (rest los) (+ 1 constant) holder)
    ))))

(define positions-of
  (lambda (s los)
    (helper s los 0 '() )))

(check-equal? (positions-of 'a '(a b a c d a e f g a h i j k)) '(0 2 5 9))
(check-equal? (positions-of 'a '(a b b b b b b b b b b)) '(0))
(check-equal? (positions-of 'a '(b b c d e f g h i j k)) '())

;; --------------------------------------------------------------------------

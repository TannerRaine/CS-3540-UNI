;;  ------------------------------------------------------------------------
;; |   FILE           :  homework08-tests.rkt                               |
;; |   AUTHOR         :  Eugene Wallingford                                 |
;; |   DATE           :  2024/03/26                                         |
;; |   DESCRIPTION    :  An incomplete set of tests for the static analysis |
;; |                     functions provided with Homework 8.  They test     |
;; |                     preprocess indirectly.                             |
;;  ------------------------------------------------------------------------

#lang racket
(require rackunit)
(require "syntax-procs.rkt")
(require "set-adt.rkt")
(require "homework08.rkt")

;; --------------------------------------------------------------------------
;; (occurs-bound? v core-exp) -> boolean
;; (occurs-free?  v core-exp) -> boolean

(check-true  (occurs-bound? 'a (preprocess '(let (a b)
                                              (let (c (lambda (d) a))
                                                (if x y (c a)))))))

(check-false (occurs-bound? 'b (preprocess '(let (a b)
                                              (let (c (lambda (d) a))
                                                (if x y (c a)))))))

(check-false (occurs-free? 'a (preprocess '(let (a b)
                                             (let (c (lambda (d) a))
                                               (if x y (c a)))))))

(check-true (occurs-free? 'b (preprocess '(let (a b)
                                            (let (c (lambda (d) a))
                                              (if x y (c a)))))))
;; 
;; ;; --------------------------------------------------------------------------
;; (free-vars core-exp) -> set

(check set-equal? (free-vars 'x)
                  '(x))
(check set-equal? (free-vars '(square x))
                  '(square x))
(check set-equal? (free-vars '(if x
                                  (lambda (y) (f y))
                                  (lambda (z) (g z))))
                  '(x f g))
(check set-equal? (free-vars '(lambda (y) (x y)))
                  '(x))
(check set-equal? (free-vars '((lambda (y) (y (square x)))
                               (lambda (y) (f y))))
                  '(square x f))
(check set-equal? (free-vars (preprocess '(let (a b)
                                            (let (c (lambda (d) a))
                                              (c a)))))
             
                  '(b))
(check set-equal? (free-vars (preprocess '(let (a (f b))
                                            (if a (c a) (d a)))))
             
                  '(c d f b))

;; --------------------------------------------------------------------------

;;---------------------------------------------------------------------------

;; (

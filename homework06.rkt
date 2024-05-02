;;
;; FILE:     homework06.rkt
;; AUTHOR:   Tanner Raine
;; DATE:     2/27/2024
;; COMMENT:  This module defines the five functions specified in
;;           Homework 6 as an importable module.  Use the function
;;           stub if you do not have a working solution of your own.
;;
;; MODIFIED: 2/29/2024
;; CHANGE:   problem 1 code
;;

#lang racket
(require "syntax-procs.rkt")
(require "occurs-procs.rkt")
(provide tails
         n-list?
         tree-min
         declared-vars
         prefix->postfix
)

;; --------------------------------------------------------------------------
;; Problem 1                                           (structural recursion)
;; --------------------------------------------------------------------------

(define tails
  (lambda (lst)
    (cond
      [(empty? lst) (cons lst null)] ;empty list
      [(cons lst (tails (rest lst)))]
      )))

;; --------------------------------------------------------------------------
;; Problem 2                                               (mutual recursion)
;; --------------------------------------------------------------------------

(define n-list?
  (lambda (obj)
    (cond
      [(empty? obj) #t]
      [(not (and (num-expr? (first obj))
                 (n-list? (rest obj)))) #f]
      [#t]
      )))
         

(define num-expr?
  (lambda (obj)
    (cond
      [(symbol? obj) #f]
      [(list? obj) (n-list? obj)]
      [(number? obj) #t]
       )))

;; --------------------------------------------------------------------------
;; Problem 3                                           (structural recursion)
;; --------------------------------------------------------------------------

(define tree-min
         (lambda (bin-tree)
           (cond
             [(empty? bin-tree) +inf.0]
             [(list? (first bin-tree)) (and (tree-min (rest bin-tree)) (tree-min (first bin-tree)))]
             [(exact-round (min (car bin-tree) (tree-min (rest bin-tree)) ))])))
    

;; --------------------------------------------------------------------------
;; Problem 4                                                (little language)
;; --------------------------------------------------------------------------

(define (declared-vars exp)
  (cond ((varref? exp)
        '())
        ((lambda? exp)
         (cons (lambda->param exp)
               (declared-vars (lambda->body exp))))
        ((app? exp)
         (append (declared-vars (app->proc exp))
                (declared-vars (app->arg exp))))
        (else
         (error "Invalid expression in declared-vars"))))


;; --------------------------------------------------------------------------
;; Problem 5                                                (little language)
;; --------------------------------------------------------------------------

(define prefix->postfix
  (lambda (exp)
    'x))           ; only a default value...

;; --------------------------------------------------------------------------


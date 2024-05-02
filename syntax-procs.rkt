;;  ------------------------------------------------------------------------
;; |   FILE           :  syntax-procs.rkt                                   |
;; |   AUTHOR         :  Eugene Wallingford                                 |
;; |   CREATION DATE  :  2024/02/24                                         |
;; |   DESCRIPTION    :  These functions implement syntax procedures for    |
;; |                     a simple language grammar consisting only of       |
;; |                     variable references, lambda expressions, and       |
;; |                     function applications.                             |
;;  ------------------------------------------------------------------------
;; |   MODIFIED       :  2024/03/19                                         |
;; |   DESCRIPTION    :  Added a let expression to the little language and  |
;; |                     a utility function at the top of the file.         |
;;  ------------------------------------------------------------------------

#lang racket
(provide exp?
         varref?   make-varref
         lambda?   make-lambda     lambda->param lambda->body
         app?      make-app        app->proc     app->arg

         ; NEW FEATURE: a simple let expression
         let?      make-let        let->var      let->val       let->body)

;;  ------------------------------------------------------------------------
;;   This code works with the following grammar:
;;
;;        <exp>      ::= <varref>
;;                     | ( lambda ( <var> ) <exp> )
;;                     | ( <exp> <exp> )
;;                     | ( let (<var> <exp>) <exp> )       <--- NEW FEATURE
;;  ------------------------------------------------------------------------

;;  ------------------------------------------------------------------------
;;  general type predicate

(define exp?
  (lambda (exp)
    (or (varref? exp)
        (lambda? exp)
        (app?    exp)
        (let?    exp))))        ; <--- NEW FEATURE

;;  ------------------------------------------------------------------------
;;  varrefs

(define varref? symbol?)

(define make-varref identity)

;;  ------------------------------------------------------------------------
;;  lambda expressions

(define lambda?
  (lambda (exp)
    (and (list? exp)
         (= (length exp) 3)
         ; ---------------------------------- lambda keyword
         (eq? (first exp) 'lambda)
         ; ---------------------------------- parameter list
         (list? (second exp))
         (= 1 (length (second exp)))
         (symbol? (first (second exp)))
         ; ---------------------------------- body
         (exp? (third exp)))))

(define lambda->param caadr)
(define lambda->body  third)

(define make-lambda
  (lambda (parameter body)
    (list 'lambda (list parameter) body)))

;;  ------------------------------------------------------------------------
;;  application expressions  ("apps")

(define app?
  (lambda (exp)
    (and (list? exp)
         (= (length exp) 2)
         (exp? (first exp))
         (exp? (second exp)))))

(define app->proc first)
(define app->arg  second)

(define make-app
  (lambda (fn arg)
    (list fn arg)))

;;  ------------------------------------------------------------------------
;;  let expressions          * NEW FEATURE *

(define let?
  (lambda (exp)
    (and (list? exp)
         (= (length exp) 3)
         (eq? 'let (first exp))
         (binding? (second exp))
         (exp? (third exp)))))

(define binding?
  (lambda (exp)
    (and (list? exp)
         (= (length exp) 2)
         (varref? (first exp))
         (exp? (second exp)))))

(define let->var
  (lambda (let-exp)
    (first (second let-exp))))

(define let->val
  (lambda (let-exp)
    (second (second let-exp))))

(define let->body third)

(define make-let
  (lambda (var val body)
    (list 'let (list var val) body)))

;;  ------------------------------------------------------------------------
;;  some test code for let expressions

; (define nested-let  '(let (a b)
;                        (let (c (lambda (d) a))
;                          (c a))))
; (let? nested-let)
; (let->var nested-let)
; (let->val nested-let)
; (let->body nested-let)
; (let? (let->body nested-let))
; (let->var (let->body nested-let))
; (let->val (let->body nested-let))
; (let->body (let->body nested-let))

;; ----- END OF FILE -----
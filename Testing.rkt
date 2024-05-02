#lang racket
(require "syntax-procs.rkt")
(require "set-adt.rkt")
(provide (all-defined-out))   ; exports every function defined in the file



(define is-declared?
  (lambda (v exp)
    (cond
      [(varref? exp) #f]
      [(lambda? exp) (ormap (member? v (lambda->params exp))
                            (is-declared? v (lambda->body exp)))]
      [(app? exp) (or
                   (is-declared? v (app->proc exp))
                   (ormap eq? v (app->args exp)))]
      [else error "INCORRECT FORMAT"])))


(define member?
  (lambda (var exp)
    (if (member var exp)
        #t
        #f)))
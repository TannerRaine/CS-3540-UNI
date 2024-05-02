;;  ------------------------------------------------------------------------
;; |   FILE           :  homework08.rkt                                     |
;; |   AUTHOR         :  Eugene Wallingford                                 |
;; |   CREATION DATE  :  2024/03/26                                         |
;; |   DESCRIPTION    :  a preprocessor for our little language, now with   |
;; |                     if expressions, and static analysis functions      |
;; |                     that analyze programs in the core language         |
;;  ------------------------------------------------------------------------
;; |   AUTHOR         :  Tanner Raine                                       |
;; |   DATE           :  2024/03/26                                         |
;; |   DESCRIPTION    :  DESCRIBE YOUR CHANGES FOR HOMEWORK 8               |
;;  ------------------------------------------------------------------------

#lang racket
(require "syntax-procs.rkt")
(require "set-adt.rkt")
(provide (all-defined-out))   ; exports every function defined in the file


;;  ------------------------------------------------------------------------
;;   This code works with the following grammar:
;;
;;                       --------------------------- CORE FEATURES
;;        <exp>      ::= <varref>
;;                     | ( lambda ( <var>* ) <exp> )
;;                     | ( <exp> <exp>* )
;;                     | ( if <exp> <exp> <exp> )
;;                       --------------------------- ABSTRACTIONS
;;                     | ( let (<var> <exp>) <exp> )
;;                     | ( and <exp> <exp> )         -- new
;;                     | ( or <exp> <exp> )          -- new
;;  ------------------------------------------------------------------------

;; --------------------------------------------------------------------------
;; preprocess :: full-exp -> core-exp
;; --------------------------------------------------------------------------

(define preprocess
  (lambda (exp)
    (cond
      ;
      ; preprocess features in core language
      ;
      ( (varref? exp) (make-varref exp) )
      ( (lambda? exp)
           (make-lambda (lambda->params exp)
                        (preprocess (lambda->body exp))) )
      ( (app? exp)
           (make-app (preprocess (app->proc exp))
                     (map preprocess (app->args exp))) )
      ( (if? exp)
           (make-if (preprocess (if->test exp))
                    (preprocess (if->then exp))
                    (preprocess (if->else exp))) )
      ;
      ; preprocess abstractions into core language
      ;
      ( (let? exp)
           (let ((var  (let->var  exp))
                 (val  (let->val  exp))
                 (body (let->body exp)))
             (make-app (make-lambda (list var) (preprocess body))
                       (list (preprocess val)))) )

      [ (and? exp) ;and exp

         (make-if (and->arg1 exp) (list (and->arg2 exp)) 'FALSE)]

      [(or? exp) ;or exp
       (make-if (or->arg1 exp) 'TRUE (or->arg2 exp))]
          
      ( else (error 'preprocess? "invalid exp ~a" exp)))))


;; --------------------------------------------------------------------------
;; functions that analyze expressions in the core language
;; --------------------------------------------------------------------------

;; (is-declared? v core-exp) -> boolean

(define is-declared?
  (lambda (v core-exp)
    (cond
      [(lambda? core-exp) (or (member v (lambda->params core-exp))
                              (is-declared? v (lambda->body core-exp)))]
      
      [(app? core-exp) (or 
                        (is-declared? v (app->proc core-exp))
                        (ormap eq? (list v) (app->args core-exp)))]

      
      [(varref? core-exp) core-exp]
      
      [else error "whoops"])))

;; (define is-declared-help
;;         (lambda (v exp)
;;         (member? 
;;           ))



(is-declared? 'x  (quote ( (lambda (y) y)
                      (lambda (x) x))) )   ; but x is declared here
(is-declared? 'x (quote (lambda (x) x)) )
(is-declared? 'x (quote (lambda (y) d)))
(is-declared? 'y (quote (f (lambda (y) x)))) ;not recongnizng the lambda,
;; leading to an error since not seen as a  lambda, sends lambda back as, ((lambda (y) x))



;; (occurs-bound? v core-exp) -> boolean

(define occurs-bound?
  (lambda (s exp)
    (cond ((varref? exp) #f)
          ((lambda? exp) (or (occurs-bound? s (lambda->body exp))
                             (and (member s (lambda->params exp))
                                  (occurs-free? s (lambda->body exp)))))
          ((app? exp)    (or (occurs-bound? s (app->proc exp))
                             (ormap (lambda (arg)
                                      (occurs-bound? s arg))
                                    (app->args exp))))
          ((if? exp)     (or (occurs-bound? s (if->test exp))
                             (occurs-bound? s (if->then exp))
                             (occurs-bound? s (if->else exp))))
          (else (error 'occurs-bound? "invalid exp ~a" exp)))))

;; (occurs-free? v core-exp) -> boolean

(define occurs-free?
  (lambda (s exp)
    (cond ((varref? exp) (eq? s exp))
          ((lambda? exp) (and (not (member s (lambda->params exp)))
                              (occurs-free? s (lambda->body exp))))
          ((app? exp)    (or (occurs-free? s (app->proc exp))
                             (ormap (lambda (arg)
                                      (occurs-free? s arg))
                                    (app->args exp))))
          ((if? exp)     (or (occurs-free? s (if->test exp))
                             (occurs-free? s (if->then exp))
                             (occurs-free? s (if->else exp))))
          (else (error 'occurs-free? "invalid exp ~a" exp)))))

;; (free-vars core-exp) -> set

(define free-vars
  (lambda (exp)
    (cond ((varref? exp)
              (set exp) )
          ((lambda? exp)
              (set-minus (free-vars (lambda->body exp))
                         (lambda->params exp)) )
          ((app? exp)
               (set-union (free-vars (app->proc exp))
                          (set-union-all (map free-vars (app->args exp)))))
          ((if? exp)
              (set-union (free-vars (if->test exp))
                         (set-union (free-vars (if->then exp))
                                    (free-vars (if->else exp)))) )
          (else (error 'free-vars "invalid exp ~a" exp)))))

;; --------------------------------------------------------------------------

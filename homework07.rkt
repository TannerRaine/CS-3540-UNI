;;  ------------------------------------------------------------------------
;; |   FILE           :  homework07.rkt                                     |
;; |   AUTHOR         :  Tanner Raine                                       |
;; |   CREATION DATE  :  3/19/2024                                          |
;; |   DESCRIPTION    :  [ YOUR COMMENT ]                                   |
;;  ------------------------------------------------------------------------

#lang racket
(require "syntax-procs.rkt")
(provide curry
         empty-set   set-member?   set-add
         set-union
         set-subset?
         free-vars
         preprocess)               ; I provide this from class, for testing

;; --------------------------------------------------------------------------
;; Problem 1                                           (structural recursion)
;; --------------------------------------------------------------------------

;DONE

(define curry
  (lambda (lambda-exp)
    (curry-helper (second lambda-exp) (cdr (cdr lambda-exp))) ))

(define curry-helper
  (lambda (var body) ;body singifies the rest of the function
    (if (< 1 (length var))
        (append (cons 'lambda (list (list (first var)))) (list (curry-helper (rest var) body))) ;greater than 1, do some recursive stuff
        (append (append '(lambda) (list var)) body ))
    ))


    

;; --------------------------------------------------------------------------
;; Problem 2                                        (non-recursive solutions)
;; --------------------------------------------------------------------------


;DONE

(define empty-set ;returns an empty set (empty list)
  (lambda ()
    '()
    ))

(define set-member? ;returns #t if sym is in S, else #f
  (lambda (sym S)
    (if (member sym S)
        #t
        #f)
    ))

(define set-add
  (lambda (sym S)
    (if (set-member? sym S)
        S
        (cons sym S))

        ))

;; --------------------------------------------------------------------------
;; Problem 3                                           (structural recursion)
;; --------------------------------------------------------------------------

;DONE

(define set-union
  (lambda (S1 S2)
    (set-helper S1 S2 '()) ))

(define set-helper
  (lambda (S1 S2 List)
    (if (null? S1)
        (if (null? S2) ;both empty, return set
            List
            (if (member (first S2) List)
                (set-helper S1 (rest S2) List) ;S2 Item already in List
                (set-helper S1 (rest S2) (set-add (first S2) List) ))) ;S2 not in list already
        (if (member (first S1) List)
            (set-helper (rest S1) S2 List)
            (set-helper (rest S1) S2 (set-add (first S1) List))))
    ))


;; --------------------------------------------------------------------------
;; Problem 4                                           (structural recursion)
;; --------------------------------------------------------------------------

;DONE

(define set-subset?
  (lambda (S1 S2)
    (if (set-sub-help S1 S2)
        #t
        #f)))

(define set-sub-help
  (lambda (S1 S2)
    (cond
      [(null? S1) #t] ;got through all of S1, no #f set
      [(null? S2) #f]
      [(member (first S1) S2) (set-sub-help (rest S1) S2)]
      [else #f])
       ))

;; --------------------------------------------------------------------------
;; Problem 5                                           (structural recursion)
;; --------------------------------------------------------------------------


(define free-vars
  (lambda (exp)
    (cond

      [(varref? exp) (set-add exp (empty-set))]

      [(lambda? exp) (remove (lambda->param exp) (free-vars (lambda->body exp)))]

      [(app? exp) (set-union (free-vars (app->proc exp))
                       (free-vars(app->arg exp)))]
      
      [else error "Invalid Format :("])
    ))




;; --------------------------------------------------------------------------
;; preprocess :: full-exp -> core-exp          (for use in testing Problem 5)
;; --------------------------------------------------------------------------

(define preprocess
  (lambda (exp)
    (cond
      ( (varref? exp) (make-varref exp) )
      ( (lambda? exp)
           (make-lambda (lambda->param exp)
                        (preprocess (lambda->body exp))) )
      ( (app? exp)
           (make-app (preprocess (app->proc exp))
                     (preprocess (app->arg  exp))) )
      ( else  ;; let
           (let ((var  (let->var  exp))
                 (val  (let->val  exp))
                 (body (let->body exp)))
             (make-app (make-lambda var (preprocess body))
                       (preprocess val)) ) ))))

;; --------------------------------------------------------------------------
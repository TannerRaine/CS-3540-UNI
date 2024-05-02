#lang racket

(require "utilities.rkt")
(provide (all-defined-out))
#| 
       <color> ::= (rgb <byte> <byte> <byte> )
                 | ( <unary-op> <color> )
                 | ( <color> <2color-op> <color> )
                 | ( <color> <1color-op> <number> )

                 <unary-op> ::= invert | darker
                 <2color-op> ::= + | - | mix
                 <1color-op> ::= * | shift                 |# ;;Syntax

;----------------GENERAL TYPE PREDICATE---------------------------------------


(define color? ;general type predicate
  (lambda (exp)
    (or (rgb?          exp)
        (mix?          exp)
        (2color-op?    exp)
        (1color-op?    exp)
        (darker?       exp)
        (unary-op?     exp)
        )))


;----------------MIX---------------------------------------
;; (color1 mix color2)
;;((color1 * 0.5) + (color2 * 0.5))


(define mix? ;type predicate (AFTER PRE-PROCESS)
  (lambda (item)
    (and
     (= (length item) 3)
     (color? (first item))
     (eq? 'mix (second item))
     (color? (third item)))))




;----------------DARKER---------------------------------------
;(color * 0.5)


(define darker? ;type predicate (AFTER PRE-PROCESS)
  (lambda (item)
    (let ( (list-of-symbols '(*)))
    (and
     (darker-test item)
     (color? (first item))
     (member? list-of-symbols (second item))
     (eq? (third item) 0.5)
     ))))

(define darker-test
  (lambda (item)
     (= (length item) 3)))

(define darker-color ;accessor
  (lambda (item)
    (first item)))

(define darker-cons ;constructor
  (lambda (color)
    (list color '* '0.5)))
    

;----------------UNARY-OP---------------------------------------
;( <unary-op> <color> )


(define unary-op? ;predicate
  (lambda (item)
    (let ( (list-of-unary '(invert darker)) )
    (and
     (or
      (member? list-of-unary (first item))
      (darker? item))
     (color? (second item))
     ))))

(define unary-op-symbol ;accessor operation
  (lambda (item)
    (first item)))

(define unary-op-color ;accessor color
  (lambda (item)
    (second item)))

(define unary-op-cons ;constructor
  (lambda (op color)
    (list op color)))


;----------------RGB---------------------------------------
;(rgb <byte> <byte> <byte> )

(define rgb? ;type predicate
  (lambda (item)
    (if (eq? 'rgb (first item)) ;RGB
        (and (andmap number? (rest item)) ;Numbers
             (andmap byte? (rest item)) ;Bytes
             (eq? 3 (length (rest item)))) ;Correct amnt of nums
    #f)))

(define color-byte1 ;accessor color byte 1
  (lambda (item)
    (second item)))

(define color-byte2 ;accessor color byte 2
  (lambda (item)
    (third item)))

(define color-byte3 ;accessor color byte 3
  (lambda (item)
    (fourth item)))

(define color-cons ;constructor
  (lambda (byte1 byte2 byte3)
    (list 'rgb byte1 byte2 byte3)))
    


;----------------2COLOR-OP---------------------------------------
;| ( <color> <2color-op> <color> )

(define 2color-op? ;type predicate
  (lambda (item)
    (let ( (list-of-symbols '(+ - mix)) )
      (and
       (2color-op-helper item)
       (color? (first item))
       (member? list-of-symbols (2color-op-symbol item))
       (color? (third item))
       ))))

(define 2color-op-helper
  (lambda (item)
    (= (length item) 3)))

(define 2color-op-color1 ;accessor color 1
  (lambda (item)
    (first item)))

(define 2color-op-symbol ;accessor symbol
  (lambda (item)
    (second item)))

(define 2color-op-color2 ;accessor color 2
  (lambda (item)
    (third item)))

(define 2color-op-cons ;constructor
  (lambda (color 2color-op color2)
    (cons color (list 2color-op color2))))


;----------------1COLOR-OP---------------------------------------
;| ( <color> <1color-op> <number> )

(define 1color-op? ;predicate
  (lambda (item)
    (let ( (list-of-symbols '(* shift)) )
      (and
       (1color-op-helper item)
       (color? (first item))
       (member? list-of-symbols (second item))
       (number? (third item))
       ))))

(define 1color-op-helper
  (lambda (exp)
    (= (length exp) 3)))

(define 1color-op-cons ;constructor
  (lambda (color color-op number)
    (cons color (list color-op number))))

(define 1color-op-color ;accessor color
  (lambda (item)
    (first item)))

(define 1color-op-symbol ;accessor symbol
  (lambda (item)
    (second item)))

(define 1color-op-number ;accessor number
  (lambda (item)
    (third item)))

;----------------PRE-PROCESS---------------------------------------
;; (color1 mix color2)
;; ... which is a syntactic abstraction of ((color1 * 0.5) + (color2 * 0.5))
;; (darker color)
;; ... which is a syntactic abstraction of (color * 0.5)


(define preprocess
  (lambda (sugared-exp)
    (cond
      [(mix-pre sugared-exp) (let ( (color1 (first sugared-exp)) (mix (second sugared-exp)) (color2 (third sugared-exp)))
                               (list (list (preprocess color1) '* '0.5) '+ (list (preprocess color2) '* '0.5) ))]
      [(darker-pre sugared-exp) (let ((color (second sugared-exp)))
                                  (list (preprocess color) '* '0.5))]

      [(color? sugared-exp) (cond
                              [(rgb? sugared-exp) sugared-exp]
                              [(unary-op? sugared-exp) (list (first sugared-exp) (preprocess (rest sugared-exp)))]
                              [(2color-op? sugared-exp) (list (preprocess (first sugared-exp)) (second sugared-exp) (preprocess (third sugared-exp)))]
                              [(1color-op? sugared-exp) (list (preprocess (first sugared-exp)) (second sugared-exp) (third sugared-exp))])
                            ]

      [else error "This doesn't work."])))
      
(define mix-pre ;checks if mix prior to sugar removed
  (lambda (item)
    (and
     (= (length item) 3)
     (color? (first item))
     (color? (third item))
     (eq? 'mix (second item)))))

(define darker-pre
  (lambda (item)
    (and
     (= (length item) 2)
     (eq? 'darker (first item))
     (color? (second item)))))
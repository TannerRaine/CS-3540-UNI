;;
;; FILE:     datatypes.rkt
;; AUTHOR:   Tanner Raine
;; DATE:     2024/04/23
;; COMMENT:  Datatypes used with interpreter.rkt, for things such as binding list
;;           
;;
;; MODIFIED: 2024/04/23 by Tanner Raine
;; CHANGE:   Fixed look-up calls
;;

#lang racket

(require "utilities.rkt")
(provide (all-defined-out))


;----------------BINDINGS---------------------------------------

(define (make-bindings)
  '())

(define bind
  (lambda (name val list)
    (cons (cons name val) list)))

(define (look-up name env)
  (let ((binding (assoc name env)))
    (if binding
        (cdr binding)
        (error "environment: undefined variable --" name))))

(define var-exists?
  (lambda (name env)
    (cond
      [(null? name) #f]
      [(assoc name env) #t]
      [else #f])))

;; (define env
;;   (bind 'white '(rgb 255 255 255) (bind 'black '(rgb 0 0 0) (make-bindings) )))

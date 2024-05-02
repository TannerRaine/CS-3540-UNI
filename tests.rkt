;;
;; FILE:     tests.rkt
;; AUTHOR:   Tanner Raine
;; DATE:     2024/04/23
;; COMMENT:  Tests for interpreter.rkt, syntax-procs.rkt, datatypes.rkt & utilities.rkt
;;           
;;
;; MODIFIED: 2024/04/23 by Tanner Raine
;; CHANGE:   Added test cases for color/in?
;; ALL TESTS FROM LINE 22 THROUGH 103 ARE WORK OF EUGENE WALLINGFORD

#lang racket

(require rackunit)
(require "syntax-procs.rkt")
(require "interpreter.rkt")

;; ------------------------------------------------------------------------
;; syntax procedures
;; ------------------------------------------------------------------------

(check-true  (unary-op? 'invert))
(check-true  (unary-op? 'darker))
(check-false (unary-op-core? 'darker))
(check-true  (2color-op? '+))
(check-true  (2color-op? '-))
(check-true  (2color-op? 'mix))
(check-false (2color-op-core? 'mix))
(check-true  (1color-op? '*))
(check-true  (1color-op? 'shift))
(check-false (1color-op-core? 'mix))

(check-true  (rgb? '(rgb   1  23 234)))
(check-true  (rgb? '(rgb 255 255 255)))
(check-true  (rgb? '(rgb   0   0   0)))
(check-false (rgb? '(rgb  -1  25 123)))
(check-false (rgb? '(rgb   1 256 255)))

(check-true  (unary-exp? '(invert (rgb 150 99 42))))
(check-true  (unary-exp? '(darker (rgb 150 99 42))))
(check-true  (unary-exp? '(darker ((rgb 150 99 42) + (rgb 50 18 241)))))
(check-false (unary-exp? '((rgb 150 99 42) + (rgb 50 18 241))))

(check-true  (2color-exp? '((rgb 150 99 42) + (rgb 50 18 241))))
(check-true  (2color-exp? '((rgb 4 4 4)  +
                            ((rgb 150 99 42) mix (rgb 50 108 21)))))
(check-true  (2color-exp? '((rgb 255 0 255) mix ((rgb 0 255 0) +
                                                 (rgb 4 4 4)))))
(check-false (2color-exp? '(darker (rgb 150 99 42))))

(check-true  (1color-exp? '((rgb 255 0 255) * 1.2)))
(check-true  (1color-exp? '((rgb 255 0 255) shift -10)))
(check-false (1color-exp? '((rgb 150 99 42) + (rgb 50 18 241))))

   ; The rgb constructor has to do real work.  It is worth testing.

(check-equal? (rgb   1   2   3)    '(rgb   1 2 3))
(check-equal? (rgb 261   2   3)    '(rgb 255 2 3))
(check-equal? (rgb   1  -2   3)    '(rgb   1 0 3))
(check-equal? (rgb 175.0 2.5 3.14) '(rgb 175 2 3))

(check-exn exn:fail? (lambda () (rgb 1 2 'a))  "arg of wrong type") 
(check-exn exn:fail? (lambda () (rgb 1 2 3 4)) "too many args")
(check-exn exn:fail? (lambda () (rgb 1 2))     "too few  args")

;; ------------------------------------------------------------------------
;; preprocessor
;; ------------------------------------------------------------------------

(check-equal? (preprocess '(invert (rgb 150 99 42)))
              '(invert (rgb 150 99 42)))
(check-equal? (preprocess '(darker (rgb 150 99 42)))
              '((rgb 150 99 42) * 0.5))
(check-equal? (preprocess '((rgb 4 4 4) +
                           ((rgb 150 99 42) mix (rgb 50 108 21))))
              '((rgb 4 4 4) +
               (((rgb 150 99 42) * 0.5) + ((rgb 50 108 21) * 0.5))))

   ; These tests delve into the error cases for preprocessing
   ; illegal expressions.  Those cases will not be reached by
   ; our interpreter under ordinary conditions.  But the
   ; original versions had bugs in them!

(check-exn exn:fail? (lambda ()
                       (preprocess '((rgb 1 2 3) + 3)))
                     "shift not +")

;; ------------------------------------------------------------------------
;; evaluator
;; ------------------------------------------------------------------------

(check-equal? (eval-exp '(invert (rgb 150 99 42)))
              '(rgb 105 156 213))
(check-equal? (eval-exp '((rgb 150 99 42) + (rgb 50 18 241)))
              '(rgb 200 117 255))
(check-equal? (eval-exp '((rgb 255 0 255) mix ((rgb 0 255 0) + (rgb 4 4 4))))
              '(rgb 129 127 129))

(check-exn exn:fail? (lambda ()
                       (eval-exp '((rgb 0 0 0) + 3)))
                     "shift not +")

;; ------------------------------------------------------------------------

;; ------------------------------------------------------------------------
;; Color/in
;; ------------------------------------------------------------------------

(check-equal? (eval-exp 'white)
'(rgb 255 255 255))

(check-equal? (eval-exp '(color red = (rgb 255 0 0) in
                (darker red)))
              '(rgb 127 0 0))

(check-equal? (eval-exp '(color red = (rgb 255 0 0) in
                (color pink = (white mix red) in
                  (darker pink))))
'(rgb 127 63 63))



;; ------------------------------------------------------------------------


;; ------------------------------------------------------------------------
;; Do ;;Scooby-Doo :-)
;; ------------------------------------------------------------------------

(check-equal? (eval-exp 'white)
'(rgb 255 255 255))

(check-equal? (eval-exp '(do (rgb 255 0 0)))
'(rgb 255 0 0))

(check-equal? (eval-exp '(color c = (rgb 0 255 0) in
                (do (c <= (c mix (rgb 0 0 255)))
                    (c <= (invert c))
                    (darker (c shift 5)))))
'(rgb 127 66 66))

(check-equal? (eval-exp '(color c = (rgb 0 255 0) in
                (color d = (rgb 0 0 255) in
                  (do (c <= (c mix d))
                      (d <= (c mix d))
                      ((c mix d) shift 5)))))
'(rgb 5 99 163))


;; ------------------------------------------------------------------------
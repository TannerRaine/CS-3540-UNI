#lang racket

(require "syntax-procs.rkt")
(require "utilities.rkt")
(require racket/trace)

(define eval-exp
  (lambda (exp)

    (cond
      [(color? exp) (eval-exp-helper (preprocess exp))]
      [else error "Invalid Format :("])))


(define eval-exp-helper
  (lambda (exp)

    (cond

      [(rgb? exp) exp] ;nothing to eval

      [(unary-op? exp) (unary-op-exec exp)]

      [(2color-op? exp) (2color-op-exec exp)]

      [(1color-op? exp) (1color-op-exec exp)]

      [else error "huey: illegal expression --" exp])


    ))

(define 2color-op-exec
  (lambda (exp)
    (let ( (color1 (eval-exp (first exp))) (color2 (eval-exp (third exp))))

      (cond
        ;;[(not (and (rgb? color1) (rgb? color2))) (eval-exp-helper (2color-op-cons (eval-exp-helper color1) (second exp) (eval-exp-helper color2)))) ]

        [(eq? '+ (second exp))
         (list 'rgb
               (byte-check (+ (second color1) (second color2)))
               (byte-check (+ (third color1) (third color2)))
               (byte-check (+ (fourth color1) (fourth color2))))]

        [(eq? '- (second exp))
         (list 'rgb
               (byte-check (- (second color1) (second color2)))
               (byte-check (- (third color1) (third color2)))
               (byte-check (- (fourth color1) (fourth color2))))]

        [(eq? '* (second exp))
         (list 'rgb
               (exact-floor (+ (* (second color1) 0.5) (* (second color2) 0.5)))
               (exact-floor (+ (* (third color1) 0.5) (* (third color2) 0.5)))
               (exact-floor (+ (* (fourth color1) 0.5) (* (fourth color2) 0.5))))]

        [else error "Invalid Format"]))))

;(trace 2color-op-exec)

(define 1color-op-exec
  (lambda (exp)
    (let ( (color (eval-exp-helper (first exp))) (number (third exp)) )
      (cond

        ;;[(not (rgb? color)) (eval-exp-helper color)]

        [(eq? (second exp) '*)
         (list 'rgb
               (byte-check (exact-floor (* (second color) number)))
               (byte-check (exact-floor (* (third color) number)))
               (byte-check (exact-floor (* (fourth color) number))))]

        [(eq? (second exp) 'shift)
         (list 'rgb
               (byte-check (exact-floor (+ (second color) number)))
               (byte-check (exact-floor (+ (third color) number)))
               (byte-check (exact-floor (+ (fourth color) number))))]

        [else error "Invalid Format"]))))

(define unary-op-exec
  (lambda (exp)
    (let ( (color (second exp)))
      (cond
        [(not (rgb? color)) (eval-exp-helper (second exp))]
        [(eq? 'invert (first exp))
         (list 'rgb
               (- 255 (second color))
               (- 255 (third color))
               (- 255 (fourth color)))]

        [(eq? '* (second exp))
         (list 'rgb
               (exact-floor (* 0.5 (second color)))
               (exact-floor (* 0.5 (third color)))
               (exact-floor (* 0.5 (fourth color))))]

        [else error "Invalid Format"]))))

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
                              [(unary-op? sugared-exp) (list (first sugared-exp) (preprocess (second sugared-exp)))]
                              [(2color-op? sugared-exp) (list (preprocess (first sugared-exp)) (second sugared-exp) (preprocess (third sugared-exp)))]
                              [(1color-op? sugared-exp) (list (preprocess (first sugared-exp)) (second sugared-exp) (third sugared-exp))])
                            ]
      [(number? sugared-exp) sugared-exp]

      [else error "This doesn't work."])))
      
(define mix-pre ;checks if mix prior to sugar removed
  (lambda (item)
    (and
     (= (length item) 3)
     (eq? 'mix (second item))
     (color? (first item))
     (color? (third item)))))

(define darker-pre
  (lambda (item)
    (and
     (= (length item) 2)
     (eq? 'darker (first item))
     (color? (second item)))))
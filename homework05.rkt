;;
;; FILE:     homework05.rkt
;; AUTHOR:   Tanner Raine
;; DATE:     2/21/2024
;; COMMENT:  This module defines the five functions specified in
;;           Homework 5 as an importable module.  Notice how each
;;           each function has a body and returns a default value.
;;           Writing stubs of this sort enables us to load the file
;;           and run tests, even if the tests fail.
;;
;; MODIFIED: 2/22/2024
;; CHANGE:   Added code to Problem 5 with notes
;;

#lang racket
(provide prefix-of? includes-course? nlist+ max-length prefix->infix)
(require racket/trace)

;; --------------------------------------------------------------------------
;; Problem 1                                           (structural recursion)
;; --------------------------------------------------------------------------

(define prefix-of?
  (lambda (lst1 lst2)
    (if (empty? lst1)
        #t ;lst1 empty, thus true
        (if (empty? lst2)
            #f ;lst2 empty, thus false
            (if (not (eq? (first lst1) (first lst2)))
                #f ;one of the items from lst1 doesn't match lst2, false
                (prefix-of? (rest lst1) (rest lst2)))))))
      

;; --------------------------------------------------------------------------
;; Problem 2                                               (mutual recursion)
;; --------------------------------------------------------------------------

(define includes-course?
  (lambda (course requirements)
    (if (empty? requirements)
        #f
        (or (includes-course-ce? course (first requirements))
            (includes-course? course (rest requirements))))))

(define includes-course-ce?
  (lambda (course course-exp)
    (if (symbol? course-exp)
        (eq? course course-exp) ;returns #t or #f since symbol
        (includes-course? course course-exp)))) ;not symbol (nested list)

;; --------------------------------------------------------------------------
;; Problem 3                                               (mutual recursion)
;; --------------------------------------------------------------------------

(define nlist+
  (lambda (nlst)
    (if (empty? nlst)
        0 ;empty lst
        (+ (num-expr+ (first nlst) )
            (nlist+ (rest nlst))))))

(define num-expr+
  (lambda (num)
    (if (number? num)
        num ;if num, just return
        (nlist+ num)))) ;nested list


;; --------------------------------------------------------------------------
;; Problem 4                                               (mutual recursion)
;; --------------------------------------------------------------------------

(define max-length
  (lambda (str-list)
    (if (empty? str-list)
        -1 ;empty list
        (max (max-length-se (first str-list))
             (max-length (rest str-list))))))

(define max-length-se
  (lambda (str)
    (if (string? str)
        (string-length str) ;return length
        (max-length str)))) ;else, recurisve call back into main
        

;; --------------------------------------------------------------------------
;; Problem 5                                               (mutual recursion)
;; --------------------------------------------------------------------------



(define prefix->infix
  (lambda (binary-ex)
    (if (empty? binary-ex)
        '() ;empty list
        (or (helper binary-ex)
            (prefix->infix (rest binary-ex))))))

(define helper
  (lambda (lst)
    (if (number? (second lst)) ;if num, just cons
        (cons (second lst) (list (first lst) (third lst)))
        (cons (prefix->infix (second lst)) (list (first lst) (prefix->infix (third lst))))))) ;not num, nested lists 
   

;; --------------------------------------------------------------------------

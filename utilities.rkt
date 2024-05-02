#lang racket


(provide (all-defined-out))   ; exports every function defined in the file

(define member? ;;Like member, but returns #t if found and #f otherwise
  (lambda (list-of item)
    (if (member item list-of)
        #t
        #f)))

(define byte-check
  (lambda (exp)
    (if (byte? exp)
        exp
        (if (> exp 255)
            255
            0))))

;;number? is already in Racket
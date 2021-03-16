#lang racket

(provide (all-defined-out))

; dynamic typing: use values of any type anywhere
(define xs (list 4 5 6))    
(define ys (list (list 4 (list 5 0)) 6 7 (list 8) 9 2 3 (list 0 1)))

(define (sum1 xs)
    (if (null? xs) 
        0 
        (if (number? (car xs)) 
            (+ (car xs) (sum1 (cdr xs)))
            (+ (sum1 (car xs)) (sum1 (cdr xs))))))

(define (sum2 xs)
    (if (null? xs) 
        0
        (if (number? (car xs))
            (+ (car xs) (sum1 (cdr xs)))
            (if (list? (car xs)) 
                (+ (sum2 (car xs)) (sum2 (cdr xs)))
                (sum2 (cdr xs))))))


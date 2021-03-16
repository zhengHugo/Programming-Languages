#lang racket

(provide (all-defined-out))

(define (f x) (+ x (* x b))) ; forward reference ok
(define b 3)
(define c (+ b 4)) ; backward reference ok
(define e (+ e 4)) ; not okay
(define e 5)




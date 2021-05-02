#lang racket

(provide (all-defined-out))

; 1 1 1 1 ...
(define ones (lambda () (cons 1 ones)))

; 1 2 3 4 5
(define nats
  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

(define powers-of-two 
  (letrec ([f (lambda (x) (cons x (lambda () (f (* x 2)))))])
    (lambda () (f 1))))

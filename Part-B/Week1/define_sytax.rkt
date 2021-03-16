#lang racket

(provide (all-defined-out))

(define-syntax my-if
  (syntax-rules (then else)
    [(my-if e1 then e2 else e3) (if e1 e2 e3)]))
  
; replace with an expression with another one
(define-syntax comment-out 
  (syntax-rules () 
    [(comment-out ignore instead)]))

(define-syntax my-delay
  (syntax-rules ()
    [(my-delay e) (mcons #f (lambda () e))]))
  
(define (my-force p)
  (if (mcar p)
    (mcdr p)
    (begin 
      (set-mcar! p #t)
      (set-mcdr! p ((mcdr p)))
      (mcdr p))))

(define-syntax my-force-macro
  (syntax-rules ()
    [(my-force e)
      (let ([x e])
        (if (mcar x)
          (mcdr x)
          (begin 
            (set-mcar! x #t)
            (set-mcdr! x ((mcdr x))))))]))
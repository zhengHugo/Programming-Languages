#lang racket
(provide all-defined-out)

; multiply x and the result of y-thunk, calling y-thunk x times
(define (my-mult x y-thunk)
  (cond [(= x 0) 0] 
        [(= x 1) (y-thunk)] 
        [#t (+ (y-thunk) (my-mult (- x 1) y-thunk))]))

(define (my-delay th)
  (mcons #f th))

(define (my-force p)
  (if (mcar p)
      (mcdr p)
      (begin (set-mcar! p #t)
             (set-mcdr! p ((mcdr p)))
             (mcdr p))))


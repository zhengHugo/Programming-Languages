
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; helper functions
(define ones (lambda () (cons 1 ones)))

;; Problem 1
(define (sequence low high stride)
  (if (> low high)
    '()
    (cons low (sequence (+ low stride) high stride))))

;; Problem 2
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) 
    xs))

;; Problem 3
(define (list-nth-mod xs n)
  (cond 
    [(< n 0) (error "list-nth-mod: negative number")] 
    [(null? xs) (error "list-nth-mod: empty list")] 
    [#t (car 
      (list-tail xs (remainder n (length xs))))]))

;; Problem 4
(define (stream-for-n-steps s n) 
  (if (= n 0) 
    '()
    (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

;; Problem 5
(define funny-number-stream 
  (letrec 
    ([f 
      (lambda (x) 
        (if (= (remainder x 5) 0)
          (cons (- x) (lambda () (f (+ x 1))))
          (cons x (lambda () (f (+ x 1))))))])
    (lambda () (f 1))))

;; Problem 6
(define (dan-then-dog)
   (cons "dan.jpg" (lambda () (cons "dog.jpg" dan-then-dog))))

;; Problem 7
(define (stream-add-zero s)
  (lambda () 
    (cons 
      (cons 0 (car (s))) 
      (stream-add-zero (cdr (s))))))


;; Problem 8
(define (cycle-lists xs ys)
  (letrec 
    ([f 
      (lambda (x)
        (cons 
          (cons (list-nth-mod xs x) (list-nth-mod ys x)) 
          (lambda () (f (+ x 1)))))])
    (lambda ()(f 0))))
  
;; Problem 9
(define (vector-assoc v vec)
  (letrec 
    ([helper (lambda (n)
      (cond 
      [(= n (vector-length vec)) #f]
      [(and (pair? (vector-ref vec n)) (equal? v (car (vector-ref vec n))))
        (vector-ref vec n)]
      [#t (helper (+ n 1))]))])
    (helper 0)))

;; Problem 10
(define (cached-assoc xs n)
  (letrec (
    [memo (make-vector n)]
    [i 0]) ; vector of pair '(value, ans) 
    (lambda (v) 
      (let 
        ([ans (vector-assoc v memo)])
        (if ans 
          (cdr ans) 
          (let ([new-ans (assoc v xs)])
            (begin 
              ;;; (vector-set! memo i (cons v new-ans))
              (vector-set! memo i new-ans)
              (set! i (modulo (+ i 1) n))
              new-ans)))))))

;; Problem 11
(define-syntax while-less
  (syntax-rules (do)
  [(while-less e1 do e2) 
   (letrec ([ans1 e1] 
    [f (lambda () ; call f to evaluate e2 
        (let ([ans2 e2])
          (if (< ans2 ans1) (f) #t)))])
    (f))]))
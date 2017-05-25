#lang sicp

(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2))
(define (improve guess x) (average guess (/ x guess)))
(define (good-enough? guess x)
  (< (abs (- (square guess) x))
     0.001))
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))
(define (sqrt1 x) (sqrt-iter 1.0 x))

;(sqrt1 100000000000000000)         ; do not converge!
(sqrt  100000000000000000)

(define (close-enough? a b)
  (< (abs (- a b))
     0.001))
(define (sqrt-iter2 guess x)
  (if (close-enough? guess (improve guess x))
    guess
    (sqrt-iter2 (improve guess x) x)))
(define (sqrt2 x) (sqrt-iter2 1.0 x))

(sqrt2 100000000000000000)

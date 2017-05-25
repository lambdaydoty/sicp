#lang sicp

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

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

(sqrt1 9)
(sqrt1 2)

(new-if (= 2 3) 0 5)
(new-if (= 1 1) 0 5)
(define (sqrt-iter2 guess x)
  (new-if (good-enough? guess x)
    guess
    (sqrt-iter2 (improve guess x) x)))
(define (sqrt2 x) (sqrt-iter2 1.0 x))

(sqrt2 9)
(sqrt2 2)

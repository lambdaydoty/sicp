#lang sicp

;; A program consists of separate procedures:
(define (square x)              (* x x))
(define (average x y)           (/ (+ x y) 2))
(define (improve      guess x)  (average guess (/ x guess)))
(define (good-enough? guess x)  (< (abs (- (square guess) x)) 0.001))
(define (sqrt-iter    guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))
(define (sqrt1 x)
  (sqrt-iter 1.0 x))

(sqrt1 2.0)

;; the program translated into BLOCK STRUCTURE
(define (sqrt2 x)
  (define (square x)              (* x x))
  (define (average x y)           (/ (+ x y) 2))
  (define (improve      guess x)  (average guess (/ x guess)))
  (define (good-enough? guess x)  (< (abs (- (square guess) x)) 0.001))
  (define (sqrt-iter    guess x)
    (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))
  (sqrt-iter 1.0 x))

(sqrt2 2.0)

;; the program using LEXICAL SCOPING! (x is free inside the body of sqrt3)
(define (sqrt3 x)
  (define (square x)              (* x x))
  (define (average x y)           (/ (+ x y) 2))
  (define (improve      guess)  (average guess (/ x guess)))
  (define (good-enough? guess)  (< (abs (- (square guess) x)) 0.001))
  (define (sqrt-iter    guess)
    (if (good-enough? guess)
      guess
      (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

(sqrt3 2.0)

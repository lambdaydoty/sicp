#lang sicp

(define (cube x) (* x x x))
(define (improve y x) (/ (+ (/ x (* y y)) 
                            (* 2 y) )
                         3))
(define (good-enough? guess x)
  (< (abs (- (cube guess) x))
     0.001))
(define (cube-root-iter guess x)
  (if (good-enough? guess x)
    guess
    (cube-root-iter (improve guess x) x)))
(define (cube-root x) (cube-root-iter 1.0 x))

(cube-root 1.0)
(cube-root 2.0)
(cube-root 8.0)
(cube-root 27.0)


#lang racket
(require racket/trace)

;; Numerical Integral:
;;
;;  b                            dx         dx            dx
;; S        f(x)dx  =  dx ( f(a+---) + f(a+---+dx) + f(a+---+2dx) + ..._
;;  a                            2          2             2



(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(define (sum-integers a b)
  (sum (lambda (x) x)
       a
       (lambda (n) (+ n 1))
       b))
(define (sum-cubes a b)
  (sum (lambda (x) (* x x x))
       a
       (lambda (n) (+ n 1))
       b))
(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (n) (+ n 4))
       b))
(define (integral f a b dx)
  (* (sum f
          (+ a (/ dx 2))
          (lambda (t) (+ t dx))
          b)
     dx))

(sum-integers 1 10)
(sum-cubes 1 10)
(* 8 (pi-sum 1 1000))
(integral (lambda (x) (* x x x)) 0 1 0.01)
(integral (lambda (x) (* x x x)) 0 1 0.001)
(newline)

;; Simpson's Rule:
;;
;;  x_0+2h                  ( f_0 + 4 f_1 + f_2)
;; S        f(x)dx  =~    h --------------------
;;  x_0                              3
(define (simpson f a b n)
  (let ((0h (* 0 (/ (- b a) n)))
        (1h (* 1 (/ (- b a) n)))
        (2h (* 2 (/ (- b a) n)))
        (3h (* 3 (/ (- b a) n))))
    (* (sum (lambda (t) (/ (+ (* 1 (f (+ t 0h)))
                              (* 4 (f (+ t 1h)))
                              (* 1 (f (+ t 2h))))
                           3))
            a
            (lambda (t) (+ t 2h))
            b)
       1h)))
(exact->inexact (simpson (lambda (x) (* x x x)) 0 1 100))
(exact->inexact (simpson (lambda (x) (* x x x)) 0 1 1000))



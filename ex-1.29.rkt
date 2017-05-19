#lang racket
(require racket/trace)

;;  x_0+2h                h
;; S        f(x)dx  =~  -----( f_0 + 4 f_1 + f_2)
;;  x_0                   3



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

(sum-integers 1 10)
(sum-cubes 1 10)
(* 8 (pi-sum 1 1000))

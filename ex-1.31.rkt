#lang racket
(require racket/trace)


;(define (sum term a next b)
;  (if (> a b)
;    0
;    (+ (term a)
;       (sum term (next a) next b))))

(define (product1 term a next b)
  (if (> a b)
    1
    (* (term a)
       (product1 term (next a) next b))))

(define (product-integers1 a b)
  (product1 (lambda (x) x)
           a
           (lambda (n) (+ n 1))
           b))
(define (factorial1 n) (product-integers1 1 n))
(define (wallis-pi1 a b)
  (product1 (lambda (x) (/ (* (+ x 0) (+ x 2))
                           (* (+ x 1) (+ x 1))))
            a
            (lambda (n) (+ n 2))
            b))

(trace product1)
(factorial1 6)
(exact->inexact (* 4 (wallis-pi1 2 10)))
(newline)


;; iterative

(define (product2 term a next b)
  (define (iter start p)
    (if (> start b)
      p
      (iter (next start) (* p (term start)))))
  (trace iter)
  (iter a 1))

(define (product-integers2 a b)
  (product2 (lambda (x) x)
           a
           (lambda (n) (+ n 1))
           b))
(define (factorial2 n) (product-integers2 1 n))
(define (wallis-pi2 a b)
  (product2 (lambda (x) (/ (* (+ x 0) (+ x 2))
                           (* (+ x 1) (+ x 1))))
            a
            (lambda (n) (+ n 2))
            b))

(factorial2 6)
(exact->inexact (* 4 (wallis-pi2 2 10)))
(newline)


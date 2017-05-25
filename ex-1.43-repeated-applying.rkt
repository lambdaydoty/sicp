#lang racket
(require racket/trace)


(define (compose f g)
  (lambda (arg)
    (f
      (g arg))))


(define (inc x) (+ x 1))
(define (square x) (* x x))
(define (id x) x)

(define (repeated f n)
  (if (= n 0)
    id
    (compose f (repeated f (- n 1)))))

((repeated square 2) 5)


;; recall ex-1.32-accumulate.rkt
(define (accumulate1 combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a) (accumulate1 combiner null-value term (next a) next b))))
(define (repeated1 f n)
  (accumulate1 compose id (lambda (i) f) 1 (lambda (n) (+ n 1)) n))
((repeated1 square 2) 5)

#lang racket
(require racket/trace)


(define (compose f g)
  (lambda (arg)
    (f
      (g arg))))



(define (inc x) (+ x 1))
(define (square x) (* x x))
((compose square inc) 6)


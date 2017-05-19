#lang racket
(require racket/trace)

(define (inc x) (+ x 1))
(define (dec x) (- x 1))

(define (plus1 a b)
  (if (= a 0)
    b
    (inc (plus1 (dec a) b))))

(define (plus2 a b)
  (if (= a 0)
    b
    (plus2 (dec a) (inc b))))

(trace plus1)
(trace plus2)

(plus1 4 5) ; generates recursive process
(plus2 4 5) ; generates iterative process


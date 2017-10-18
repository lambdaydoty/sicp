#lang racket

(define (make-accumulator sum)
  (lambda (inc)
    (begin (set! sum (+ sum inc))
           sum)))

(define A (make-accumulator 5))
(define B (make-accumulator 5))
(A 10)
(A 10)
(B 0)

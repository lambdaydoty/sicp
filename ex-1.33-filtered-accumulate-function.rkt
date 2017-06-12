#lang racket
(require racket/trace)

(define (filtered-accumulate filter? combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (if (filter? a) (term a) null-value)
              (filtered-accumulate filter? combiner null-value term (next a) next b))))

(define (demo filtered-accumulate)
  (define (prime? n)
    (define (smallest-divisor n)
      (define (find-divisor n test-divisor)
        (define (square x) (* x x))
        (define (divides? a b) (= (remainder b a) 0))
        (cond ((> (square test-divisor) n) n)
              ((divides? test-divisor n) test-divisor)
              (else (find-divisor n (+ test-divisor 1)))))
      (find-divisor n 2))
    (= (smallest-divisor n) n))
  (define (sum-prime-squares a b) (filtered-accumulate prime?             + 0 (lambda (x) (* x
                                                                                             x)) a (lambda (n) (+ n 1)) b))
  (define (product-prime-to-n  n) (filtered-accumulate (lambda (i)
                                                         (= (gcd i n) 1)) * 1 (lambda (x) x)     1 (lambda (n) (+ n 1)) n))
  (begin
    (display (sum-prime-squares 1 10))            (display " ")
    (display (product-prime-to-n  10))            (display " ")))

(demo filtered-accumulate)      (newline)



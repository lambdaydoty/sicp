#lang sicp

(define (smallest-divisor n)
  (define (find-divisor n test-divisor)
    (define (square x) (* x x))
    (define (divides? a b) (= (remainder b a) 0))
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))
; (trace find-divisor)
  (find-divisor n 2))


(define (prime? n)
  (= (smallest-divisor n) n))

(define (timed-prime-test n)
  (define (start-prime-test n start-time)
    (define (report-prime elapsed-time)
      (display " *** ")
      (display elapsed-time))
    (if (prime? n)    ;; one-armed if
      (report-prime (- (runtime) start-time))))
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(timed-prime-test 19999)

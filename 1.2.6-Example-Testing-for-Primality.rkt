#lang racket
(require racket/trace)

(define (smallest-divisor n)
  (define (find-divisor n test-divisor)
    (define (square x) (* x x))
    (define (divides? a b) (= (remainder b a) 0))
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))
  (trace find-divisor)
  (find-divisor n 2))


(define (prime? n)
  (= (smallest-divisor n) n))

;(prime? 2) (display "\n")
;(prime? 3) (display "\n")
;(prime? 4) (display "\n")
;(prime? 5) (display "\n")
;(prime? 6) (display "\n")
;(prime? 7) (display "\n")
;(prime? 8) (display "\n")
;(prime? 9) (display "\n")
;(prime? 10)(display "\n")

(prime? 2017)

;; Fermat's Little Theorem
;; If n is a prime & a < n, 
;; then a^n = a (mod n)

(define (expmod base exp m)
  (define (square x) (* x x))
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
        (else        (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a)) ; a^n =? a (mod n)
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(trace fast-prime?)
(fast-prime? 2017 3)
(fast-prime? 2011 3)

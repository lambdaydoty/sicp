#lang racket
(require racket/trace)


;; b^n = (b^2)^(n/2) = (b^(n/2))^2
;; ab^n = const
;; 
;; 
;; 
;; +---+------+-----+-----+------+-------+-------+
;; |   | 2^10 | 4^5 | 4^4 | 16^2 | 256^1 |       |
;; +---+------+-----+-----+------+-------+-------+
;; | n |   10 |   5 |   4 |    2 |     1 | 0     |
;; | a |    1 |   1 |   4 |    4 |     4 | 4*256 |
;; | b |    2 |   4 |   4 |   16 |   256 | 256   |
;; +---+------+-----+-----+------+-------+-------+
;; 
;;              n <-- (even? n) ? (n/2) : (n-1)
;;              b <-- (even? n) ? (b^2) : b
;;              a <-- (even? n) ? a     : b*a
;;
;; INVARIANT: ab^n    =    a(b^2)^(n/2) = (b*a)b^(n-1)

(define (even? n)
  (= (remainder n 2) 0))
(define (id x) x)
(define (square x) (* x x))

(define (fast-exponent b n)
  (define (iter n b a)
    (cond ((= n 0) a)
          ((even? n) (iter (/ n 2) (square b) (* 1 a)))
          (else      (iter (- n 1) (id     b) (* b a)))))
  (trace iter)
  (iter n b 1))

(define (exponent b n)
  (define (iter n b a)
    (cond ((= n 0) a)
          (else (iter (- n 1) b (* b a)))))
  (trace iter)
  (iter  n b 1))

(exponent       2 30)
(fast-exponent  2 30)

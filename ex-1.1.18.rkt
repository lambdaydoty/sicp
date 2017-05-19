#lang racket
(require racket/trace)
;; +-------+-----+-----+-----+-----+------+--------+
;; |       | 3*7 | 3*6 | 6*3 | 6*2 | 12*1 |  12*0  |
;; +-------+-----+-----+-----+-----+------+--------+
;; | n     |   7 |   5 |   3 | 2   | 1    | 0      |
;; | b     |   3 |   3 |   6 | 6   | 12   | 12     |
;; | a     |   0 |   3 |   3 | 3+6 | 3+6  | 3+6+12 |
;; | a+b*n |     |     |     |     |      |        |
;; +-------+-----+-----+-----+-----+------+--------+
;; 
;;              n <-- (even? n) ? (n/2) : (n-1)
;;              b <-- (even? n) ? (b*2) : b
;;              a <-- (even? n) ? a     : b+a
;;
;; INVARIANT:  a+b*n
(define (even? n)
  (= (remainder n 2) 0))
(define (id x) x)
(define (square x) (* x x))
(define (double x) (* x 2))
(define (halve  x) (/ x 2))
;; -------------------------------------------------------

(define (mult a b)
  (cond ((= b 0) 0)
         (else (+ a (mult a (- b 1))))))

(define (fast-mult a b)
  (cond ((= b 0) 0)
        ((even? b) (double (fast-mult a (halve b))))
        (else (+ a (fast-mult a (- b 1))))))

(define (fast-mult-iter b n)
  (define (iter n b a)
    (cond ((= n 0) a)
          ((even? n) (iter (/ n 2) (double b) (+ 0 a)))
          (else      (iter (- n 1) (id     b) (+ b a)))))
  (trace iter)
  (iter n b 0))

(trace mult)
(trace fast-mult)
(trace fast-mult-iter)
(mult           7 68)
(fast-mult      7 68)
(fast-mult-iter 7 68)

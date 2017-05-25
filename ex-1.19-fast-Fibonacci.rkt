#lang racket
(require racket/trace)


;;
;; a <- bq + aq + ap
;; b <- bp + aq
;;
;; a <- b + a       ; (p q) = (0 1)
;; b <- a           ; (p q) = (0 1)
;;
;; p' = p^2 + q + q^2
;; q' = 2pq
;;
;;

(define (even? n) (= (remainder n 2) 0))


(define (super-fib  n)
  (define (iter a b p q c)
    (define (transform-p p) (+ (* p p) (* q q)))
    (define (transform-q q) (+ (* q q) (* 2 p q)))
    (define (Tpq-a a b) (+ (* b q)
                           (* a q)
                           (* a p)))
    (define (Tpq-b a b) (+ (* b p)
                           (* a q)))
    (cond ((= c 0) b)
          ((even? c) (iter a
                           b
                           (transform-p p)  ;; Tpq^2
                           (transform-q q)  ;; Tpq^2
                           (/ c 2)))
          (else      (iter (Tpq-a a b)
                           (Tpq-b a b)
                           p
                           q
                           (- c 1)))))
  (trace iter)
  (iter 1 0 0 1 n))

;;

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))
(define (fast-fib n)
  (define (iter a b n)
    (cond ((= n 0) b)
          ((= n 1) a)
          (else (iter (+ a b) a (- n 1)))))
  (trace iter)
  (iter 1 0 n))

(trace fib)
(trace fast-fib)
(trace super-fib)

(fib        9)
(fast-fib   9)
(super-fib  9)
(display "\n")
(fast-fib   33)
(super-fib  33)

#lang racket
(require racket/trace)
(define (average x y z) (/ (+ x y z) 3))


(define (compose f g)
  (lambda (arg)
    (f
      (g arg))))

(define (repeated f n)
  (if (= n 0)
    (lambda (x) x)
    (compose f (repeated f (- n 1)))))


(define (smooth f)
  (define dx 0.00001)
  (lambda (x)
    (average (f (- x dx))
             (f x)
             (f (+ x dx)))))

(define (n-fold-smooth f n)  
  ((repeated smooth n) f)) 

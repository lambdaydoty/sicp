#lang racket
(require racket/trace)




(define cons0
  (lambda (x)
    (lambda (y)
      (lambda (m)
        (if m x y)))))
(define (car0 z) (z #t))
(define (cdr0 z) (z #f))

(define my-pair ((cons0 6) 8))
my-pair
(car0 my-pair)
(cdr0 my-pair)

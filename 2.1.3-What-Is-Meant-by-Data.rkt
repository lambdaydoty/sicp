#lang racket

(define (cons0 x y)
  (lambda (m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "123")))))

(define (car0 z) (z 0))
(define (cdr0 z) (z 1))


(cons0 6 8)

(define my (cons0 6 8))

(car0 my)
(cdr0 my)

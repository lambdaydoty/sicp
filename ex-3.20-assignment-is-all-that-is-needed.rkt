#lang racket

(define (cons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (lambda (m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          ((eq? m 'set-car!) set-x!)
          ((eq? m 'set-cdr!) set-y!)
          (else (error "Undefined operation -- CONS" m)))))
(define (car z) (z 'car))
(define (cdr z) (z 'cdr))
(define (set-car! z v) ((z 'set-car!) v))
(define (set-cdr! z v) ((z 'set-cdr!) v))



(define x (cons 1 2))
(define z (cons x x))
(car x)

(set-car! (cdr z) 17)

(car x)

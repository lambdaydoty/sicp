#lang racket

; An alternative way to address the problem of reducing rationals
; to lowest terms is to perform the reduction whenever we access
; the parts of a rational, rather than when we construct it.
(define (make-rat n d) (cons n d))
(define (numer z)       (let ((g (gcd n d)))
                          (/ (car z) g))
(define (denom z)       (let ((g (gcd n d)))
                          (/ (cdr z) g))

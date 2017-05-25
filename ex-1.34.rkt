#lang racket
(require racket/trace)


(define (f g) (g 2))
(f (lambda (x) (* x x)))
(f (lambda (z) (* z (+ z 1))))
(f f) ; (X) application: not a procedure;


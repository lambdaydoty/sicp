#lang racket
(require "3.3.4--Representing-wires.rkt")
(require "3.3.4--Primitive-function-boxes.rkt") (require "ex-3.28-or-gate.rkt")
(provide half-adder)
(provide or-gate1)
(define or-gate1 or-gate)


(define a (make-wire))
(define b (make-wire))
(define c (make-wire))
(define d (make-wire))
(define e (make-wire))
(define s (make-wire))

(define (half-adder a b s c)
  (let ([d (make-wire)]
        [e (make-wire)])
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))


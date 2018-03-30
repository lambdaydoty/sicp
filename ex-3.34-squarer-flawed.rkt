#lang racket
(require "3.3.5-Propagation-of-Constraints--Representing-connectors.rkt")
(require "3.3.5-Propagation-of-Constraints--Implementing-the-constraint-system.rkt")

;; Create the network:
(define (squarer a b)
    (multiplier a a b)
  'ok)

(define A (make-connector))
(define B (make-connector))
(squarer A B)
(probe "The square-root" A)
(probe "The square" B)

;; demo
(set-value! A 11 'user)
(forget-value! A 'user)
(set-value! B 169 'user)
(display "??")
;(set-value! B 20 'user)
;(forget-value! A 'user)
;(set-value! A 30 'user)

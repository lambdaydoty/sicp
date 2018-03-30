#lang racket
(require "3.3.5-Propagation-of-Constraints--Representing-connectors.rkt")
(require "3.3.5-Propagation-of-Constraints--Implementing-the-constraint-system.rkt")

;; Create the network:
(define (averager a b c)
  (let ([u (make-connector)]
        [v (make-connector)])
    (adder a b u)
    (multiplier c v u)
    (constant 2 v))
  'ok)

(define A (make-connector))
(define B (make-connector))
(define C (make-connector))
(averager A B C)
(probe "The average" C)

;; demo
(set-value! A 10 'user)
(set-value! B 20 'user)
(forget-value! A 'user)
(set-value! A 30 'user)

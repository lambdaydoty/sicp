#lang racket
(require "3.3.4--Representing-wires.rkt")
(require "3.3.4--Implementing-the-agenda.rkt")
(require "3.3.4-A-Simulator-for-Digital-Circuits.rkt")
(require "3.3.4--The-agenda.rkt")
(require compatibility/mlist)



(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display " New-value = ")
                 (display (get-signal wire)))))

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(check-agenda-detail)

(probe 'sum-wire sum)
(probe 'carry-wire carry)

(half-adder input-1 input-2 sum carry)      (check-agenda-detail)

(set-signal! input-1 1)                     (check-agenda-detail)
(propagate)

(set-signal! input-2 1)                     (check-agenda-detail)
(propagate)


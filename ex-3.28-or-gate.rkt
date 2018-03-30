#lang racket
(require "3.3.4--Representing-wires.rkt")
(require "3.3.4--The-agenda.rkt")
(provide or-gate)


(define or-gate-delay 5)

(define (or-gate a1 a2 output)
  (define (logical-or s t)
    (cond ((and [= s 0] [= t 0]) 0)
          ((and [= s 0] [= t 1]) 1)
          ((and [= s 1] [= t 0]) 1)
          ((and [= s 1] [= t 1]) 1)
          (else (error "Invalid signal" s t))))
  (define (or-action-procedure)
    (let ([new-value
            (logical-or (get-signal a1)
                        (get-signal a2))])
      (after-delay or-gate-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

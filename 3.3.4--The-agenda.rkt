#lang racket
(require "3.3.4--Implementing-the-agenda.rkt")
(provide the-agenda)
(provide after-delay)
(provide propagate)
(provide check-agenda-detail)

(define the-agenda (make-agenda))

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
    'done
    (let ([first-item (first-agenda-item the-agenda)])
      (first-item)
      (remove-first-agenda-item! the-agenda)
      (propagate))))

(define (check-agenda-detail)
  (print-agenda the-agenda))

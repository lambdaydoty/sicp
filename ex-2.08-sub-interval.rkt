#lang racket
(require "ex-2.07-interval-selectors.rkt")
(require "2.1.4-Extended-Exercise-Interval-Arithmetic.rkt")
(provide sub-interval)


(define (sub-interval x y)
  (add-interval
    x
    (make-interval (- 0.0 (upper-bound y))
                   (- 0.0 (lower-bound y)))))



;;;; ----------------------------------------------
(define int1 (make-interval 1.1 2.3))
(define int2 (make-interval 2.0 4.0))
(print-interval (sub-interval int1 int2))

#lang racket
(require racket/trace)
(define (average x y) (/ (+ x y) 2))

;; fixed-point
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2) (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (trace try)
  (try first-guess))

(fixed-point (lambda (y) (+ 1
                            (/ 1 y)))
             1.0)                               (newline)


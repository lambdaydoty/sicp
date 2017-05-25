#lang racket
(require racket/trace)
(define (average x y) (/ (+ x y) 2))

;; fixed-point
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2) (< (abs (- v1 v2)) tolerance))
  (define (try guess counter)
    (display counter)
    (display ") ")
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next (+ 1 counter)))))
  (try first-guess 1))

(fixed-point (lambda (y) (/ (log 1000)
                            (log y)))
             2.0)                               (newline)
(fixed-point (lambda (y) (average y
                                  (/ (log 1000)
                                     (log y))))
             2.0)                               (newline)


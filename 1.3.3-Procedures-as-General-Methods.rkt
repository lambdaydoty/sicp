#lang racket
(require racket/trace)
(define (average x y) (/ (+ x y) 2))

;; half-interval-method
(define (search f neg-point pos-point)
  (define (close-enough? x y) (< (abs (- x y)) 0.001))
  (define (positive? x) (> x 0))
  (define (negative? x) (< x 0))
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
      midpoint
      (let ((test-value (f midpoint)))
        (cond ((positive? test-value) (search f neg-point midpoint))
              ((negative? test-value) (search f midpoint pos-point))
              (else                   midpoint))))))
(trace search)
(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value)) (search f a b))
          ((and (negative? b-value) (positive? a-value)) (search f b a))
          (else (error "Values are not of opposite sign" a b)))))

(half-interval-method sin 2.0 4.0)      (newline)
(half-interval-method (lambda (x) (- (* x x x)
                                     (* 2 x)
                                     3))
                      1.0
                      2.0)              (newline)

;; fixed-point
(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2) (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

(fixed-point cos 1.0)                           (newline)
(fixed-point (lambda (y) (+ (sin y)
                            (cos y)))
             1.0)                               (newline)
(fixed-point (lambda (y) (average y (/ 2 y)))
             1.0)                               (newline)


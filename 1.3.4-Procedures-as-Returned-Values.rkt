#lang racket
(require racket/trace)
(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2) (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))




(define (average-damp f)
  (define (average x y) (/ (+ x y) 2))
  (lambda (x) (average x (f x))))

(define (newton-transform g)
  (define (deriv g)
    (define dx 0.00001)
    (lambda (x) (/ (- (g (+ x dx))
                      (g x))
                   dx)))
  (lambda (x) (- x (/ (g x)
                      ((deriv g) x)))))


(define (square-root x)  (fixed-point (average-damp     (lambda (y) (/ x y)))         1.0))
(define (cube-root x)    (fixed-point (average-damp     (lambda (y) (/ x (* y y))))   1.0))
(define (square-root2 x) (fixed-point (newton-transform (lambda (y) (- (* y y) x)))   1.0))
(define (cube-root2 x)   (fixed-point (newton-transform (lambda (y) (- (* y y y) x))) 1.0))
;(define (fixed-point-of-transform g transform guess)
;                        (fixed-point (transform        g)                          guess))

(square-root  2)
(cube-root    2)        (newline)
(square-root2 2)        
(cube-root2   2)        (newline)

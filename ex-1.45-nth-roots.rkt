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
(define (repeated f n)
  (define (id x) x)
  (define (compose f g)
    (lambda (arg) (f (g arg))))
  (if (= n 0)
    id
    (compose f (repeated f (- n 1)))))

(define (seed x n)
  (lambda (y)
    (/ x
       ((repeated (lambda (t) (* y t)) (- n 1)) 1))))


(define (square-root x)  (fixed-point (average-damp
                                        (seed x 2))         1.0))
(define (cube-root x)    (fixed-point (average-damp
                                        (average-damp
                                          (seed x 3)))      1.0))
(square-root  2)
(cube-root    2)        (newline)
